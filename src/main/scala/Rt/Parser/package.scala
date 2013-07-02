package Rt
import scalaz._
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

  def rtStatusMatcher( status: String ) = {
    ("""RT/4.\d+.\d+ """ + status).r
  }
  private val OkRe =
    rtStatusMatcher("200 Ok")
  private val AuthenticationRequiredRe =
    rtStatusMatcher("401 Authentication Required")

  private[Parser] def parseResponse(
    responseLines: List[String]
  ):ParserError \/ List[String] = {
    responseLines match {
      case OkRe()::""::rest                 => \/-(rest)
      case OkRe()::rest                     => \/-(rest)
      case AuthenticationRequiredRe()::rest => -\/(AuthenticationRequired)
      case _                                => -\/(
        BadResponse(responseLines.mkString("\n"))
      )
    }
  }

  def expectString( s:String , body:String ): ParserError \/ Unit = {
    parseResponse( body.split("\n").toList ).flatMap{
      case l::ls if s == l => \/-(())
      case resBody         => -\/(ExpectationNotMet(s,resBody.mkString("\n")))
    }
  }

  def expectRegex( r:Regex , body:String ): ParserError \/ Unit = {
    parseResponse( body.split("\n").toList ).flatMap{
      case r()::ls => \/-(())
      case resBody => -\/(ExpectationNotMet(r.toString,resBody.mkString("\n")))
    }
  }
}
