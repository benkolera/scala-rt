package Rt.Parser

import scalaz._
import syntax.applicative._
import syntax.traverse._
import std.option._
import std.list._
import com.github.nscala_time.time.Imports._
import scala.util.Try

object Ticket {

  val dtf = DateTimeFormat.forPattern("yyyy-MM-dd HH:mm:ss");
  val listRe = """\s*,\s*""".r
  def splitList( s:String ) = listRe.split( s ).toList

  def extractTicketPeople(
    m: Map[String,String]
  ): Parser[Rt.TicketPeople] = {
    (
      Field.extractField(m)("Owner").map( Some(_).filter( _ != "Nobody" ) ) |@|
      Field.extractField(m)("Creator") |@|
      Field.extractFieldList(m)("Requestors") |@|
      Field.extractFieldList(m)("Cc") |@|
      Field.extractFieldList(m)("AdminCc")
    ) { Rt.TicketPeople.apply _ }
  }

  def extractTicketPriority(
    m: Map[String,String]
  ): Parser[Rt.TicketPriority] = {
    (
      Field.extractFieldInt(m)("Priority") |@|
      Field.extractFieldInt(m)("InitialPriority") |@|
      Field.extractFieldInt(m)("FinalPriority")
    ){ Rt.TicketPriority.apply _ }
  }

  def extractTicketDates(
    m: Map[String,String]
  ): Parser[Rt.TicketDates] = {
    (
      Field.extractFieldDateTime(m)("Created") |@|
      Field.extractFieldDateTime(m)("LastUpdated") |@|
      Field.extractFieldOptDateTime(m)("Starts") |@|
      Field.extractFieldOptDateTime(m)("Started") |@|
      Field.extractFieldOptDateTime(m)("Due") |@|
      Field.extractFieldOptDateTime(m)("Resolved") |@|
      Field.extractFieldOptDateTime(m)("Told")
    ){ Rt.TicketDates.apply _ }
  }

  val numberRe = """^(\d+)$""".r
  val minutesRe = """^(\d+) minutes?$""".r
  val hoursRe = """^(\d+) hours?$""".r

  def extractTicketEffort(
    m: Map[String,String]
  ): Parser[Rt.TicketEffort] = {
    def extractEffort( fieldName:String ) =
      Field.extractField(m)(fieldName).flatMap{
      case numberRe(minutes) => minutes.toInt.point[Parser]
      case minutesRe(minutes) => minutes.toInt.point[Parser]
      case hoursRe(hours) => (hours.toDouble * 60).toInt.point[Parser]
      case s => parserFail(InvalidField(fieldName,s"Unknown effort value: $s"))
    }

    (
      extractEffort("TimeEstimated") |@|
      extractEffort("TimeWorked") |@|
      extractEffort("TimeLeft")
    ){ Rt.TicketEffort.apply _ }
  }

  val customFieldRe = """CF\.\{(.+?)\}""".r

  def extractCustomFields(m: Map[String,String]): Rt.CustomField.Map = {
    m.collect{
      case (customFieldRe(name),v) => Rt.CustomField.tuple(name,v)
    }
  }

  val ticketIdRe = """^ticket/(\d+)$""".r
  def extractTicketId( s:String ): Parser[Int] = s match {
    case ticketIdRe(id) => id.toInt.point[Parser]
    case _ => parserFail(InvalidField(
      "id",s"Expected ticket id to be of form 'ticket/{id}'. Got: $s"
    ))
  }

  def parseTicket( responseStr: String ):Parser[Rt.Ticket] = {
    parseResponse( responseStr.split("\n").toList ).flatMap(
      parseSingleTicket _
    )
  }



  def parseTickets( responseStr: String ):Parser[List[Rt.Ticket]] = {
    parseResponse( responseStr.split("\n").toList ).flatMap( lines =>
      splitMultipart( lines ).map( parseSingleTicket _ ).sequenceU
    )
  }

  private def parseSingleTicket( lines: List[String] ) = {
    Field.parseFieldMap( lines ).flatMap( fieldMap => {
      (
        Field.extractField(fieldMap)("id").flatMap(extractTicketId) |@|
          Field.extractField(fieldMap)("Queue") |@|
          Field.extractField(fieldMap)("Subject") |@|
          Field.extractField(fieldMap)("Status") |@|
          extractTicketPeople( fieldMap ) |@|
          extractTicketPriority( fieldMap ) |@|
          extractTicketDates( fieldMap ) |@|
          extractTicketEffort( fieldMap ) |@|
          extractCustomFields( fieldMap ).point[Parser]
      ){ Rt.Ticket.apply _ }
    })
  }

}
