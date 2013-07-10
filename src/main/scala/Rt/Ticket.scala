package com.benkolera.Rt

import org.joda.time.DateTime

case class TicketPeople (
  owner: Option[String],
  creator: String ,
  requestors: List[String],
  ccs: List[String],
  adminCcs: List[String]
)

case class TicketPriority (
  priority: Int,
  initialPriority: Int,
  finalPriority: Int
)

case class TicketDates (
  created: DateTime,
  lastUpdated: DateTime,
  starts: Option[DateTime],
  started: Option[DateTime],
  due: Option[DateTime],
  resolved: Option[DateTime],
  told: Option[DateTime]
)

case class TicketEffort (
  timeEstimated: Int,
  timeWorked: Int,
  timeLeft: Int
)

case class Ticket (
  id: Int ,
  queue: String ,
  subject: String,
  status: String ,
  people: TicketPeople,
  priority: TicketPriority,
  dates: TicketDates,
  effort: TicketEffort,
  customFields: CustomField.Map
)

case class NewTicket(
  queue: String,
  subject: String,
  text: String,
  status: Option[String] = None,
  owner: Option[String] = None,
  requestors: List[String] = Nil,
  ccs: List[String] = Nil,
  adminCcs: List[String] = Nil,
  priority: Option[Int] = None,
  initialPriority: Option[Int] = None,
  finalPriority: Option[Int] = None,
  starts: Option[DateTime] = None,
  started: Option[DateTime] = None,
  due: Option[DateTime] = None,
  resolved: Option[DateTime] = None,
  timeEstimated: Option[Int] = None,
  timeWorked: Option[Int] = None,
  timeLeft: Option[Int] = None,
  customFields: CustomField.Map = Map()
)

case class TicketMessageAttachment(
  fileName: String,
  data: Array[Byte],
  mimeType: String,
  charSet: String
)

case class TicketMessage(
  text: String = "",
  subject: Option[String] = None,
  attachments: List[TicketMessageAttachment] = Nil ,
  ccs: List[String] = Nil,
  bccs: List[String] = Nil,
  timeWorked: Int = 0
)

case class TicketMovement(
  oldUser: String,
  newUser: String
)

object Ticket {

  import dispatch._
  import scalaz._
  import syntax.monad._
  import scalaz.contrib.std.scalaFuture._
  import std.list._
  import std.either._
  import scala.concurrent.Future
  import scala.collection.JavaConversions._
  import scala.util.matching.Regex
  import QueryBuilder.{Query,OrderBy}

  def show(id:Int)( implicit m:Monad[Future] ):RtM[Option[Ticket]] = {
    for {
      req  <- rtApi.map( _ / "ticket" / id / "show" )
      body <- callApi( req )
      dtf  <- getDtf
      t    <- liftParseError(Parser.Ticket.parseTicket(dtf,body))
    } yield t
  }

  def history(id:Int)( implicit m:Monad[Future] ) = {
    for {
      req  <- rtApi.map( _ / "ticket" / id / "history" <<? Map("format"->"l") )
      body <- callApi( req )
      dtf  <- getDtf
      hist <- liftParseError(Parser.History.parseHistory(dtf,body))
    } yield hist
  }

  def query(query:Query,orderBy:Option[OrderBy])(
    implicit m:Monad[Future]
  ) = {
    queryRaw(
      QueryBuilder.buildQueryString( query ),
      orderBy.map( QueryBuilder.buildOrderByString( _ ) )
    )
  }

  def queryRaw(query:String,orderBy:Option[String])(
    implicit m:Monad[Future]
  ):RtM[List[Ticket]] = {
    val queryMap = ("query",query) :: ("format","l") ::
      orderBy.map( o => List("orderby" -> o) ).getOrElse(Nil)

    for {
      req  <- rtApi.map( _ / "search" / "ticket" << queryMap )
      body <- callApi( req )
      dtf  <- getDtf
      res  <- liftParseError(Parser.Ticket.parseTickets(dtf,body))
    } yield res
  }

  def create( ticket: NewTicket )( implicit m:Monad[Future] ) = {
    val c = Formatter.NewTicket.toContentString(ticket)
    for {
      req  <- rtApi.map( _ / "ticket" / "new" ).map(addContentParam(c))
      body <- callApi( req )
      id   <- liftParseError(Parser.NewTicket.parseId(body))
    } yield id
  }

  def comment( id: Int, msg: TicketMessage )( implicit m:Monad[Future] ) = {
    message( "comment" )( id,msg )
  }

  def correspond( id: Int , msg:TicketMessage )( implicit m:Monad[Future] ) = {
    message( "correspond" )( id,msg )
  }

  def take( id: Int )( implicit m:Monad[Future] ) = {
    takeStealUntake( id, "take" )
  }

  def steal( id: Int )( implicit m:Monad[Future] ) = {
    takeStealUntake( id, "steal" )
  }

  def untake( id: Int )( implicit m:Monad[Future] ) = {
    takeStealUntake( id, "untake" )
  }

  def give( id: Int , newOwner:String )( implicit m:Monad[Future] ) = {
    edit( id , Formatter.fieldsToContentString(List("Owner" -> newOwner)) )
  }

  def moveQueue( id: Int , newQueue:String )( implicit m:Monad[Future] ) = {
    edit( id , Formatter.fieldsToContentString(List("Queue" -> newQueue)) )
  }

  def update( ticket: Ticket )( implicit m:Monad[Future] ) = {
    edit( ticket.id , Formatter.Update.toContentString( ticket ) )
  }

  private def edit( id: Int , c: String )(
    implicit m:Monad[Future]
  ) = {
    for {
      req  <- rtApi.map( _ / "ticket" / id / "edit" ).map( addContentParam(c) )
      body <- callApi( req )
      _ <- liftParseError(
        Parser.expectString(s"# Ticket ${id} updated.",body)
      )
    } yield ()
  }

  private def takeStealUntake( id:Int , action: String )(
    implicit m:Monad[Future]
  ) = {
    val c = Formatter.Take.toContentString( id, action )
    for {
      req  <- rtApi.map( _ / "ticket" / id / "take" ).map(addContentParam(c))
      body <- callApi( req )
      move <- liftParseError(Parser.TicketMovement.parseTicketMovement(body))
    } yield move
  }

  private def message( action: String )( id: Int, msg: TicketMessage )(
    implicit m:Monad[Future]
  ) = {
    val parts = Formatter.TicketMessage.toParts(action,id,msg)
    for {
      req  <- rtApi.map( _ / "ticket" / id / "comment" ).map(addParts(parts))
      body <- callApi( req )
      _    <- liftParseError(Parser.expectString("# Message recorded",body))
    } yield ()
  }

}
