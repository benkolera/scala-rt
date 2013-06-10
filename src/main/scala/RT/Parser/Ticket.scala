package Rt.Parser
import scalaz._

case class Ticket ( id: Int )

object Ticket {

  def parseticket( responseStr: String ):ParserError \/ List[String] = {
    parseResponse( responseStr.split("\n").toList ).flatMap( lines =>
      Field.parseFields( lines ).map( lines =>
        ???
      )
    )
  }
}
