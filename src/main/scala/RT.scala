import dispatch._
import com.ning.http.client.{AsyncHandler,AsyncHttpClient,Request}

case class RtConfig (
  username: String,
  password: String,
  hostname: String,
  exContext: scala.concurrent.ExecutionContext,
  http:     (Req => scala.concurrent.Future[Either[Throwable,Res]])
)

object RtConfig {
  def makeConfig(username: String, password: String, hostname: String )(
    implicit ex: scala.concurrent.ExecutionContext
  ) = {
    RtConfig(
      username,
      password,
      hostname,
      ex,
      (req:Req) => Http.apply(req).either
    )
  }
}

sealed trait RtError
case class ServerError(t: Throwable) extends RtError
object LoginFailed extends RtError
object NotLoggedIn extends RtError

object Rt {

  import com.ning.http.client.Cookie
  import scalaz._
  import syntax.monad._
  import scalaz.contrib.std.scalaFuture._
  import std.list._
  import std.either._
  import scala.concurrent.{ExecutionContext,Future}
  import scala.collection.JavaConversions._

  type CookieJar = java.util.List[Cookie]
  type RtEither[+A] = EitherT[Future,RtError,A]
  type RtM[+A] = ReaderWriterStateT[RtEither,RtConfig,List[String],CookieJar,A]

  val titleRe = """<title>(.+)</title>""".r
  def rt( implicit a:Monad[Future]) = getHostname.map( hn =>
    host(hn).secure <:< Map(
      "User-Agent" -> "ScalaRT/0.01",
      "Referer"    -> s"https://$hn" //HACK: Couldn't get query to work without getting CSRF error without this.
    )
  )
  def rtApi( implicit a:Monad[Future]) = rt.map( _ / "REST" / "1.0" )

  def emptyCookieJar = new java.util.ArrayList[com.ning.http.client.Cookie]()

  def rtRws[A](
    f: (RtConfig,CookieJar) => RtEither[(List[String],A,CookieJar)]
  )( implicit a:Monad[Future] ) =
    ReaderWriterStateT[RtEither,RtConfig,List[String],CookieJar,A]( f )

  def setCookies(jar: CookieJar)( implicit m:Monad[Future] ) =
    rtRws( (r,s) => (Nil,(),jar).point[RtEither] )
  def getCookies( implicit m:Monad[Future] ) =
    rtRws( (r,s) => (Nil,s,s).point[RtEither] )

  def getUsername( implicit m:Monad[Future] ) =
    rtRws( (r,s) => (Nil,r.username,s).point[RtEither] )
  def getPassword( implicit m:Monad[Future] ) =
    rtRws( (r,s) => (Nil,r.password,s).point[RtEither] )
  def getHostname( implicit m: Monad[Future] ) =
    rtRws( (r,s) => (Nil,r.hostname,s).point[RtEither] )
  def getExecutionContext( implicit m:Monad[Future] ) =
    rtRws( (r,s) => (Nil,r.exContext,s).point[RtEither] )
  def getHttp( implicit m:Monad[Future] ) =
    rtRws( (r,s) => (Nil,r.http,s).point[RtEither] )

  def log(l:String)( implicit m:Monad[Future] ) =
    rtRws( (r,s) => (List(l),(),s).point[RtEither] )

  def fail( err: RtError )( implicit m:Monad[Future] ) =
    rtRws( (r,s) => EitherT.left(err.point[Future]) )

  def doHttp( req: Req )( implicit m:Monad[Future] ):RtM[Res] = {
    val r = req.build
    log( s"REQ: ${r}" ).flatMap( _ =>
      getExecutionContext.flatMap( ec =>
        getHttp.flatMap( http => {
          val et = EitherT(
            http(req).map(
              either => \/.fromEither(either.left.map(ServerError(_)))
            )(ec)
          )

          rtRws( (r,s) => et.map( res => (Nil,res,s) ) )
        } )
      )
    )
  }

  def callApi( req: Req )( implicit m:Monad[Future] ) = {
    for {
      cookies <- getCookies
      res     <- doHttp( cookies.foldLeft( req )( (r,c) => r.addCookie(c)) )
    } yield res
  }

  def login( implicit m:Monad[Future] ) = {
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

  def showTicket(id:Int)( implicit m:Monad[Future] ) = {
    for {
      req <- rtApi.map( _ / "ticket" / id / "show" )
      res <- callApi( req )
    } yield res.getResponseBody()
  }

  def showTicketAttachments(id:Int)( implicit m:Monad[Future] ) = {
    for {
      req <- rtApi.map( _ / "ticket" / id / "attachments" )
      res <- callApi( req )
    } yield res.getResponseBody()
  }

  def showTicketAttachment(ticketId:Int,attachId:Int)(
    implicit m:Monad[Future]
  ) = {
    for {
      req <- rtApi.map( _ / "ticket" / ticketId / "attachments" / attachId )
      res <- callApi( req )
    } yield res.getResponseBody()
  }

  def query(query:String)(
    implicit m:Monad[Future]
  ) = {
    for {
      req <- rtApi.map( _ / "search" / "ticket" ).map( _  << Map( "query" -> query ) )
      res <- callApi( req )
    } yield res.getResponseBody()
  }

  def test = {
    import scala.concurrent.ExecutionContext.Implicits.global
    (for {
      _ <- login
      ticketRes      <- showTicket(962663)
      attachmentsRes <- showTicketAttachments(962663)
      attachRes      <- showTicketAttachment(962663,5199532)
      queryRes       <- query("Queue='dev.support' AND Owner='Nobody' AND (Status='new' OR Status='open')")
    } yield (ticketRes,attachmentsRes,attachRes,queryRes)).run(
      RtConfig.makeConfig(
        "bkolera" ,
        "xxxxx" ,
        "rt.iseek.com.au"
      ),
      emptyCookieJar
    ).fold(
      err => println( "ERROR: " + err ),
      t => {
        import scalax.io._
        val ticket = Resource.fromFile("ticket.data")
        val attachments = Resource.fromFile("attachments.data")
        val attachment = Resource.fromFile("attachment.data")
        val query = Resource.fromFile("query.data")
        List(ticket,attachments,attachment,query).foreach( _.truncate(0) )
        ticket.write( t._2._1 )
        attachments.write( t._2._2 )
        attachment.write( t._2._3 )
        query.write( t._2._4 )
        t._1.foreach( println( _ ) )
        println( "DONE" )
      }
    )
  }
}
