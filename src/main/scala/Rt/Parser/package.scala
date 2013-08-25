package com.benkolera.Rt
import scalaz._
import syntax.monad._
import scala.util.matching.Regex
import scala.annotation.tailrec

package object Parser {

  type Parser[+A] = EitherT[Scalaz.Id,ParserError,A]

  private[Parser] def parserFail[A]( err: ParserError ) =
    EitherT.left( err.point[Scalaz.Id] )

  def rtStatusMatcher( status: String ) = {
    ("""RT/4.\d+.\d+ """ + status).r
  }
  private val OkRe =
    rtStatusMatcher("200 Ok")
  private val CredentialsRequiredRe =
    rtStatusMatcher("401 Credentials required")

  private[Parser] def parseResponse(
    responseLines: List[String]
  ):Parser[List[String]] = {
    responseLines match {
      case OkRe()::""::rest            => rest.point[Parser]
      case OkRe()::rest                => rest.point[Parser]
      case CredentialsRequiredRe()::xs => parserFail(CredentialsRequired)
      case _                           => parserFail(
        BadResponse(responseLines.mkString("\n"))
      )
    }
  }

  val multipartSeparator = "--"

  private[Parser] def splitMultipart( lines: List[String] ) = {
    type Output = List[List[String]]
    @tailrec
    def hlpr( lines: List[String] , out: Output ):Output =
      lines match {
        case Nil => out.reverse
        case ls  => {
          val (history,rest) = ls.span( _ != multipartSeparator )
          hlpr( rest.dropWhile( _ == multipartSeparator ) , history :: out )
        }
      }
    hlpr( lines , Nil )
  }

  def expectString( s:String , body:String ): Parser[Unit] = {
    parseResponse( body.split("\n").toList ).flatMap{
      case l::ls if s == l => ().point[Parser]
      case resBody         => parserFail(
        ExpectationNotMet(s,resBody.mkString("\n"))
      )
    }
  }

  def expectRegex( r:Regex , body:String ): Parser[Unit] = {
    parseResponse( body.split("\n").toList ).flatMap{
      case r()::ls => ().point[Parser]
      case resBody => parserFail(
        ExpectationNotMet(r.toString,resBody.mkString("\n"))
      )
    }
  }
}
