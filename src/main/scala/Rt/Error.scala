package Rt

sealed trait Error
case class ServerError(t: Throwable) extends Error
object LoginFailed extends Error
object NotLoggedIn extends Error
case class BadResponse( pe: Parser.ParserError ) extends Error
