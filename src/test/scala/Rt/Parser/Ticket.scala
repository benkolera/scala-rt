package com.benkolera.Rt.Parser

import org.specs2._
import scalaz._
import com.github.nscala_time.time.Imports._
import org.joda.time.DateTimeZone.UTC
import com.benkolera.Rt

object TicketParserSpec extends mutable.Specification {

  import scala.io.Source
  val dtf = DateTimeFormat.forPattern("yyyy-MM-dd HH:mm:ss")

  "The Ticket Parser" should {
    "Parse a ticket" in {

      val ticketDisj = Ticket.parseTicket(
        dtf,Source.fromURL(getClass.getResource("/ticket.txt")).mkString
      ).run

      val expectedTicket = Rt.Ticket(
        id = 962663,
        queue = "dev.support",
        subject = "Test Email 1",
        status = "open",
        people = Rt.TicketPeople(
          owner = None:Option[String],
          creator = "linda",
          requestors = List("test@test.com","test2@test.com"),
          ccs = List[String](),
          adminCcs = List[String]()
        ),
        priority = Rt.TicketPriority(
          priority = 100,
          initialPriority = 100,
          finalPriority = 100
        ),
        dates = Rt.TicketDates(
          created = new DateTime(2013,6,7,15,35,28,DateTimeZone.UTC),
          lastUpdated = new DateTime(2013,6,7,16,49,40,DateTimeZone.UTC),
          starts = None:Option[DateTime],
          started = Some(new DateTime(2013,6,7,16,49,40,DateTimeZone.UTC)),
          due = Some(new DateTime(2013,6,14,15,35,28,DateTimeZone.UTC)),
          resolved = None:Option[DateTime],
          told = Some(new DateTime(2013,6,7,16,49,40,DateTimeZone.UTC))
        ),
        effort = Rt.TicketEffort(
          timeEstimated = 0,
          timeWorked = 0,
          timeLeft = 0
        ),
        customFields = Map(
          Rt.CustomField.tuple(
            "Picklist",
            "This is a picklist\nSo it gets indented like this"
          ),
          Rt.CustomField.tuple(
            "Multi Line Customfield",
            List(
              "But this is a multiline thing",
              "so it fucking gets indented with something",
              " different just to be a goddamned pain in the ass."
            ).mkString("\n")
          )
        )
      )
      //It needs the toStrings for some reason. :(
      ticketDisj.map(_.toString) must_==( \/-(Some(expectedTicket).toString) )

    }
    "Not die if no ticket was found" in {
      val ticketDisj = Ticket.parseTicket(
        dtf,"RT/4.0.12 200 Ok\n\n# Ticket 198210010 does not exist."
      ).run

      ticketDisj must_==(\/-(None))
    }
    "Parse some tickets" in {
      val ticketDisj = Ticket.parseTickets(
        dtf,Source.fromURL(getClass.getResource("/tickets.txt")).mkString
      ).run

      ticketDisj.map( _.length ) must_==(\/-(3))
      ticketDisj.map( _.map(_.id) ) must_==(\/-(List(1337,1338,1339)))

    }
    "Not die if no tickets were returned" in {
      val ticketDisj = Ticket.parseTickets(
        dtf,"RT/4.0.12 200 Ok\n\nNo matching results."
      ).run

      ticketDisj.map( _.length ) must_==(\/-(0))
    }
  }

}
