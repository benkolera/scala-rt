package Rt.Parser

import scalaz._
import syntax.applicative._

object NewTicket {

  val ticketIdRe = """# Ticket (\d+) created.""".r

  def parseId( responseStr: String ):ParserError \/ Int = {
    parseResponse( responseStr.split("\n").toList ).flatMap( lines =>
      lines match {
        case ticketIdRe(id)::xs => \/-(id.toInt)
        case firstLine::xs => -\/(MissingField("Created Ticket Id"))
      }
    )
  }
}
