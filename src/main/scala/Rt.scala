package com.benkolera

import dispatch._
import com.ning.http.client.{
  AsyncHandler,
  AsyncHttpClient,
  Request,
  Part,
  Cookie,
  StringPart
}

import scalaz._
import syntax.monad._
import scalaz.contrib.std.scalaFuture._
import std.list._
import std.either._
import concurrent.{ExecutionContext,Future}
import collection.JavaConversions._
import language.higherKinds

package object Rt {

  type CookieJar = java.util.List[Cookie]
  type RtRwsT[M[+_],+A] = ReaderWriterStateT[M,Config,List[String],CookieJar,A]
  type RtRws[+A] = RtRwsT[Future,A]
  type RtEitherT[M[+_],+A] = EitherT[M,Error,A]
  type RtM[+A] = RtEitherT[RtRws,A]

  def emptyCookieJar = new java.util.ArrayList[com.ning.http.client.Cookie]()

  def login( implicit m:Monad[Future] ):RtM[Unit] = {
    def loginReq( rtHost:Req , username:String , password:String ) =
      rtHost <<? Map(
        "user" -> username ,
        "pass" -> password
      )

    def extractCookies( res: Res ) = {
      titleRe.findFirstIn( res.getResponseBody() ) match {
        case None => fail( LoginFailed )
        case Some(titleRe("Login")) => fail( LoginFailed )
        case _ => res.getCookies().point[RtM]
      }
    }

    for {
      username <- getUsername
      password <- getPassword
      rtHost   <- rt
      res      <- doHttp( loginReq(rtHost,username,password) )
      cookies  <- extractCookies( res )
      _        <- setCookies( cookies )
    } yield ()
  }

  // == INTERNAL SHARED METHODS ================================================

  private[Rt] def log(l:String)( implicit m:Monad[Future] ):RtM[Unit] =
    rtM( (r,s) => (List(l),(),s).point[Future] )

  private[Rt] def fail[A]( err: Error )( implicit m:Monad[Future] ):RtM[A] =
    EitherT.left( err.point[RtRws] )

  private[Rt] def callApi( req: Req )( implicit m:Monad[Future] ):RtM[String] = {
    for {
      cookies <- getCookies
      res     <- doHttp( cookies.foldLeft( req )( (r,c) => r.addCookie(c)) )
    } yield res.getResponseBody()
  }

  private[Rt] def rtApi(implicit a:Monad[Future]):RtM[Req] =
    rt.map( _ / "REST" / "1.0" )

  private[Rt] def addContentParam(c: String)(req: Req) = {
    req << Map( "content" -> c )
  }
  private[Rt] def addParts(ps: List[Part])(req: Req) = {
    ps.foldLeft( req )( _.addBodyPart(_) ).setMethod("POST")
  }

  private[Rt] def getDtf( implicit m:Monad[Future] ) =
    getConfig.map( _.dateTimeFormatter )
  private[Rt] def getTz( implicit m:Monad[Future] ) =
    getConfig.map( _.dateTimeZone )

  private[Rt] def liftParseError[A]( pOut: Parser.Parser[A] )(
    implicit m:Monad[Future]
  ): RtM[A] =
    EitherT( pOut.leftMap( BadResponse(_) ).run.point[RtRws] )

  // == PRIVATE METHODS ========================================================

  private def rtRws[A](
    f: (Config,CookieJar) => Future[(List[String],A,CookieJar)]
  )( implicit a:Monad[Future] ):RtRws[A] =
    ReaderWriterStateT[Future,Config,List[String],CookieJar,A]( f )

  private def rtM[A](
    f: (Config,CookieJar) => Future[(List[String],A,CookieJar)]
  )( implicit a:Monad[Future] ):RtM[A] =
    EitherT.right[RtRws,Error,A]( rtRws( f ) )

  private def setCookies(jar: CookieJar)( implicit m:Monad[Future] ) =
    rtM( (r,s) => (Nil,(),jar).point[Future] )
  private def getCookies( implicit m:Monad[Future] ) =
    rtM( (r,s) => (Nil,s,s).point[Future] )

  private def getConfig( implicit m:Monad[Future] ) =
    rtM( (r,s) => (Nil,r,s).point[Future] )
  private def getUsername( implicit m:Monad[Future] ) =
    getConfig.map( _.username )
  private def getPassword( implicit m:Monad[Future] ) =
    getConfig.map( _.password )
  private def getRtUrl( implicit m: Monad[Future] ) =
    getConfig.map( _.serverUrl )
  private def getExecutionContext( implicit m:Monad[Future] ) =
    getConfig.map( _.exContext )
  private def getHttp( implicit m:Monad[Future] ) =
    getConfig.map( _.http )

  private def doHttp( req: Req )( implicit m:Monad[Future] ):RtM[Res] = {
    val r = req.toRequest
    def liftHttpResToRtM( config: Config ):RtM[Res] = {
      EitherT.right[RtRws,Error,Either[Throwable,Res]](config.http(req).liftM[RtRwsT]).flatMap{
        case Left(t) => EitherT.left[RtRws,Error,Res](
          rtRws( (r,s) => (List(s"RES: Exception: ${t.getMessage}"),ServerError(t),s).point[Future])
        )
        case Right(res) => rtM(
          (r,s) => (List(s"RES: ${res.getResponseBody}"),res,s).point[Future]
        )
      }
    }
    def reqToString = {
      val parts = Option(r.getParts)
      val contentPart = parts.flatMap( _.collect{ case s:StringPart => s }.headOption.map(_.getValue) )
      List(
        r,
        "Parts:",
        parts.map( _.map(_.getName).mkString(",") ).getOrElse(""),
        contentPart.map( "Content Part:" + _ ).getOrElse("")
      ).mkString( " " )
    }

    for {
      _      <- log( s"REQ: ${reqToString}" )
      config <- getConfig
      out    <- liftHttpResToRtM(config)
    } yield out
  }

  private def rt( implicit a:Monad[Future] ) = getRtUrl.map( serverUrl =>
    url(serverUrl) <:< Map(
      "User-Agent" -> "ScalaRT/0.01",
      //HACK: Couldn't get query to work without getting CSRF error without this
      "Referer"    -> serverUrl
    )
  )

  private val titleRe = """<title>(.+)</title>""".r

}
