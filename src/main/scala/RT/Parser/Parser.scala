package Rt
import scalaz._

package object Parser {

  sealed trait ParserError
  object AuthenticationRequired extends ParserError
  case class BadResponse( response: String ) extends ParserError
  case class BadBodyLine( lines: List[String] , idx: Int, msg: String ) extends ParserError

  def rtStatusMatcher( status: String ) = {
    ("""RT/4.\d+.\d+ """ + status).r
  }
  val OkRe = rtStatusMatcher("200 Ok")
  val AuthenticationRequiredRe = rtStatusMatcher("401 Authentication Required")

  def parseResponse( responseLines: List[String] ):ParserError \/ List[String] = {
    responseLines match {
      case OkRe()::""::rest                 => \/-(rest)
      case OkRe()::rest                     => \/-(rest)
      case AuthenticationRequiredRe()::rest => -\/(AuthenticationRequired)
      case _                                => -\/(
        BadResponse(responseLines.mkString("\n"))
      )
    }
  }
}
