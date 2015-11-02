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
    for {
      o  <- Field.extractField(m)("Owner").map( Some(_).filter( _ != "Nobody" ) )
      c  <- Field.extractField(m)("Creator") 
      rs <- Field.extractFieldList(m)("Requestors") 
      cc <- Field.extractFieldList(m)("Cc") 
      ac <- Field.extractFieldList(m)("AdminCc")
    } yield Rt.TicketPeople.apply(o,c,rs,cc,ac)
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
    for {
      c  <- extractFieldDateTime(dtf)(m)("Created")
      lu <- extractFieldDateTime(dtf)(m)("LastUpdated") 
      ss <- extractFieldOptDateTime(dtf)(m)("Starts") 
      sd <- extractFieldOptDateTime(dtf)(m)("Started") 
      d  <- extractFieldOptDateTime(dtf)(m)("Due") 
      rr <- extractFieldOptDateTime(dtf)(m)("Resolved") 
      t  <- extractFieldOptDateTime(dtf)(m)("Told")
    } yield Rt.TicketDates.apply(c,lu,ss,sd,d,rr,t)
  }

  val numberRe = """^(\d+)$""".r
  val minutesRe = """^(\d+) minutes?$""".r
  val hoursRe = """^(\d+) hours?$""".r

  def extractTicketEffort(
    m: Map[String,String]
  ): Parser[Rt.TicketEffort] = {
    def extractEffort( fieldName:String ):Parser[Int] =
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
      case ticketNotFoundRe()::ls => none[Rt.Ticket].point[Parser]
      case ls                     => parse1Ticket(ls).map( Some(_) )
    }
  }


  val ticketsEmptyRe = """No matching results.""".r
  def parseTickets(
    dtf: DateTimeFormatter , tz: DateTimeZone , responseStr: String
  ):Parser[List[Rt.Ticket]] = {
    val parse1Ticket = parseSingleTicket(dtf,tz) _
    parseResponse( responseStr ).flatMap{
      case ticketsEmptyRe()::ls => List[Rt.Ticket]().point[Parser]
      case lines => splitMultipart( lines ).traverseU( parse1Ticket )
    }
  }

  private def parseSingleTicket(
    dtf: DateTimeFormatter , tz: DateTimeZone
  )(lines: List[String]):Parser[Rt.Ticket] = {
    Field.parseFieldMap( lines ).flatMap( fieldMap => {
      for {
        id <- Field.extractField(fieldMap)("id").flatMap(extractTicketId) 
        q  <- Field.extractField(fieldMap)("Queue") 
        sb <- Field.extractField(fieldMap)("Subject")
        s  <- Field.extractField(fieldMap)("Status")
        tp <- extractTicketPeople( fieldMap )
        p  <- extractTicketPriority( fieldMap )
        td <- extractTicketDates( dtf, tz, fieldMap )
        te <- extractTicketEffort( fieldMap )
        cf <- extractCustomFields( fieldMap ).point[Parser]
      } yield Rt.Ticket.apply(id,q,sb,s,tp,p,td,te,cf)
    })
  }

}
