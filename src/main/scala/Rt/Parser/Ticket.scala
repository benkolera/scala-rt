package com.benkolera.Rt.Parser

import com.benkolera.Rt
import scalaz._
import syntax.applicative._
import syntax.traverse._
import std.option._
import std.list._
import com.github.nscala_time.time.Imports._
import scala.util.Try
import org.joda.time.format.DateTimeFormatter
import org.joda.time.DateTimeZone
import com.github.tototoshi.csv._
import java.io.StringReader

object Ticket {

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
    dtf: DateTimeFormatter, tz:DateTimeZone, m: Map[String,String]
  ): Parser[Rt.TicketDates] = {
    val extractFieldDateTime = Field.extractFieldDateTime( tz ) _
    val extractFieldOptDateTime = Field.extractFieldOptDateTime( tz ) _
    (
      extractFieldDateTime(dtf)(m)("Created") |@|
      extractFieldDateTime(dtf)(m)("LastUpdated") |@|
      extractFieldOptDateTime(dtf)(m)("Starts") |@|
      extractFieldOptDateTime(dtf)(m)("Started") |@|
      extractFieldOptDateTime(dtf)(m)("Due") |@|
      extractFieldOptDateTime(dtf)(m)("Resolved") |@|
      extractFieldOptDateTime(dtf)(m)("Told")
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
      case (customFieldRe(n),v) => {
        /* With a picklist, you get a csv line. With a string value, you just get
         * a big string.
         * Test for a newline to determine whether it is a string or picklist.
         */
        val values =
          if (v.contains('\n'))
            List(v)
          else
            CSVReader.open(new StringReader(v)).all.flatten
        Rt.CustomFieldName(n) -> values.map( Rt.CustomFieldValue(_) )
      }
    }
  }

  val ticketIdRe = """^ticket/(\d+)$""".r
  def extractTicketId( s:String ): Parser[Int] = s match {
    case ticketIdRe(id) => id.toInt.point[Parser]
    case _ => parserFail(InvalidField(
      "id",s"Expected ticket id to be of form 'ticket/{id}'. Got: $s"
    ))
  }

  val ticketNotFoundRe = """# Ticket \d+ does not exist.""".r
  def parseTicket(
    dtf: DateTimeFormatter , tz:DateTimeZone, responseStr: String
  ):Parser[Option[Rt.Ticket]] = {
    val parse1Ticket = parseSingleTicket(dtf,tz) _
    parseResponse( responseStr ).flatMap{
      case ticketNotFoundRe()::ls => None.point[Parser]
      case ls                     => parse1Ticket(ls).map( Some(_) )
    }
  }


  val ticketsEmptyRe = """No matching results.""".r
  def parseTickets(
    dtf: DateTimeFormatter , tz: DateTimeZone , responseStr: String
  ):Parser[List[Rt.Ticket]] = {
    val parse1Ticket = parseSingleTicket(dtf,tz) _
    parseResponse( responseStr ).flatMap{
      case ticketsEmptyRe()::ls => Nil.point[Parser]
      case lines => splitMultipart( lines ).map( parse1Ticket ).sequenceU
    }
  }

  private def parseSingleTicket(
    dtf: DateTimeFormatter , tz: DateTimeZone
  )(lines: List[String]) = {
    Field.parseFieldMap( lines ).flatMap( fieldMap => {
      (
        Field.extractField(fieldMap)("id").flatMap(extractTicketId) |@|
          Field.extractField(fieldMap)("Queue") |@|
          Field.extractField(fieldMap)("Subject") |@|
          Field.extractField(fieldMap)("Status") |@|
          extractTicketPeople( fieldMap ) |@|
          extractTicketPriority( fieldMap ) |@|
          extractTicketDates( dtf, tz, fieldMap ) |@|
          extractTicketEffort( fieldMap ) |@|
          extractCustomFields( fieldMap ).point[Parser]
      ){ Rt.Ticket.apply _ }
    })
  }

}
