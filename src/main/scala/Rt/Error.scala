package com.benkolera.Rt

sealed trait Error
case class ServerError(t: Throwable) extends Error
object LoginFailed extends Error
object NotLoggedIn extends Error
case class BadResponse( pe: Parser.ParserError ) extends Error
case class InvalidPagination( msg:String ) extends Error

object Error {
  def prettyPrint( e:Error ) = e match {
    case BadResponse(pe) => Parser.ParserError.prettyPrint(pe)
    case error => error.toString
  }
}
