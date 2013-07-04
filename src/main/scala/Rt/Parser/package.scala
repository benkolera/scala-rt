package Rt
import scalaz._
import syntax.monad._
import scala.util.matching.Regex

package object Parser {

  sealed trait ParserError
  object AuthenticationRequired extends ParserError
  case class BadResponse( response: String ) extends ParserError
  case class ExpectationNotMet( expect: String, response: String ) extends ParserError
  case class BadBodyLine( lines: List[String] , idx: Int, msg: String ) extends ParserError
  case class MissingField( fieldName: String ) extends ParserError
  case class InvalidField( fieldName: String , msg: String ) extends ParserError
  case class UnknownHistoryType( historyType: String ) extends ParserError

  type Parser[+A] = EitherT[Scalaz.Id,ParserError,A]

  private[Parser] def parserFail[A]( err: ParserError ) =
    EitherT.left( err.point[Scalaz.Id] )

  def rtStatusMatcher( status: String ) = {
    ("""RT/4.\d+.\d+ """ + status).r
  }
  private val OkRe =
    rtStatusMatcher("200 Ok")
  private val AuthenticationRequiredRe =
    rtStatusMatcher("401 Authentication Required")

  private[Parser] def parseResponse(
    responseLines: List[String]
  ):Parser[List[String]] = {
    responseLines match {
      case OkRe()::""::rest               => rest.point[Parser]
      case OkRe()::rest                   => rest.point[Parser]
      case AuthenticationRequiredRe()::xs => parserFail(AuthenticationRequired)
      case _                              => parserFail(
        BadResponse(responseLines.mkString("\n"))
      )
    }
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
