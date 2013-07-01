package Rt.Parser
import scalaz._

package object TicketMovement {

  val ticketMovementRe = """# Owner changed from (\w+) to (\w+)""".r

  def parseTicketMovement( body:String ): ParserError \/ Rt.TicketMovement = {
    parseResponse( body.split("\n").toList ).flatMap{
      case ticketMovementRe(from,to)::ls => \/-(Rt.TicketMovement(from,to))
      case resBody => -\/(ExpectationNotMet(
        "# Owner changed from <from> to <to>",resBody.mkString("\n"))
      )
    }
  }
}
