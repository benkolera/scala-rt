package com.benkolera.Rt

object Attachment {

  import dispatch._
  import scalaz._
  import syntax.monad._
  import std.list._
  import std.scalaFuture._
  import scala.concurrent.Future

  def listForTicket(id:Int)( implicit m:Monad[Future] ) = {
    for {
      req  <- rtApi.map( _ / "ticket" / id / "attachments" )
      body <- callApi( req )
    } yield body
  }

  def show(ticketId:Int,attachId:Int)(implicit m:Monad[Future]) = {
    for {
      req  <- rtApi.map( _ / "ticket" / ticketId / "attachments" / attachId )
      body <- callApi( req )
    } yield body
  }

}
