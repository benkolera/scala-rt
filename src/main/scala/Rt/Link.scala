package Rt

sealed trait Link
case class ExtUrlLink( url: String ) extends Link
case class RtTicketLink( id: Int ) extends Link

case class TicketLinks (
  ticketId: Int,
  dependsOn: List[Link],
  dependedOnBy: List[Link],
  memberOf: Option[Link],
  members: List[Link],
  refersTo: List[Link],
  referredToBy: List[Link]
)

object Link {

  import dispatch._
  import scalaz._
  import syntax.monad._
  import scalaz.contrib.std.scalaFuture._
  import std.list._
  import syntax.traverse._
  import scala.concurrent.Future

  def show(ticketId:Int)( implicit m:Monad[Future] ):Rt.RtM[TicketLinks] = {
    for {
      req  <- rtApi.map( _ / "ticket" / ticketId / "links" / "show" )
      body <- callApi( req )
      ls   <- liftParseError(Parser.Link.parseShow(body))
    } yield ls
  }

  def addMembers( ticketId: Int, links: List[Link] )(
    implicit m:Monad[Future]
  ):Rt.RtM[Unit] = {
    def add( l:Link ) = {
      //Why the heck is this different from every other call??? -__-
      val params = Formatter.Link.updateToParamMap(
        ticketId = ticketId,
        rel = "HasMember",
        del = false,
        link = l
      )
      for {
        req  <- rtApi.map( _ / "ticket" / "link" << params )
        body <- callApi( req )
        _    <- liftParseError(
          Parser.expectRegex( """.+Created link \d+ HasMember \d+$""".r , body )
        )
      } yield ()
    }
    links.map( add ).sequenceU.map( _ => () )
  }

}
