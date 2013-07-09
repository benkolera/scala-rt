package Rt.Parser

import scalaz._
import syntax.std.boolean._

sealed trait ParserError
object AuthenticationRequired extends ParserError
case class BadResponse( response: String ) extends ParserError
case class ExpectationNotMet( expect: String, response: String ) extends ParserError
case class BadBodyLine( lines: List[String] , idx: Int, msg: String ) extends ParserError
case class MissingField( fieldName: String ) extends ParserError
case class InvalidField( fieldName: String , msg: String ) extends ParserError
case class UnknownHistoryType( historyType: String ) extends ParserError

object ParserError {
  def prettyPrint( pe: ParserError ) = pe match {
    case bbl@BadBodyLine(_,_,_) => prettyPrintBadBodyLine(bbl)
    case _ => pe.toString
  }

  def prettyPrintBadBodyLine( bbl: BadBodyLine ) = {
    "Unexpected line found in response body: \n|" +
    bbl.lines.zipWithIndex.map{ case (line,idx) =>
      s"${idx+1}:|$line|" + (
        (idx+1 == bbl.idx).fold( s"<==== ERROR: ${bbl.msg}" , "" )
      )
    }.mkString("\n|")
  }
}
