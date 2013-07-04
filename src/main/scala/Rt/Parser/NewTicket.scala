package Rt.Parser

import scalaz._
import syntax.applicative._

object NewTicket {

  val ticketIdRe = """# Ticket (\d+) created.""".r

  def parseId( responseStr: String ):Parser[Int] = {
    parseResponse( responseStr.split("\n").toList ).flatMap( lines =>
      lines match {
        case ticketIdRe(id)::xs => id.toInt.point[Parser]
        case firstLine::xs => parserFail(MissingField("Created Ticket Id"))
      }
    )
  }
}
