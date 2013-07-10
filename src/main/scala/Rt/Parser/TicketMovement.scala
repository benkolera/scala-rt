package com.benkolera.Rt.Parser

import com.benkolera.Rt
import scalaz._
import syntax.monad._

package object TicketMovement {

  val ticketMovementRe = """# Owner changed from (\w+) to (\w+)""".r

  def parseTicketMovement( body:String ): Parser[Rt.TicketMovement] = {
    parseResponse( body.split("\n").toList ).flatMap{
      case ticketMovementRe(f,t)::ls => Rt.TicketMovement(f,t).point[Parser]
      case resBody => parserFail(ExpectationNotMet(
        "# Owner changed from <from> to <to>",resBody.mkString("\n"))
      )
    }
  }
}
