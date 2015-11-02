package com.benkolera.Rt.Parser

import org.specs2._
import scalaz._
import com.github.nscala_time.time.Imports._
import com.benkolera.Rt

object TicketParserSpec extends mutable.Specification {

  import scala.io.Source
  val dtf = DateTimeFormat.forPattern("yyyy-MM-dd HH:mm:ss")
  val dtz = DateTimeZone.forOffsetHours(10)

  "The Ticket Parser" should {
    "Parse a ticket" in {

      val ticketDisj = Ticket.parseTicket(
        dtf,dtz,Source.fromURL(getClass.getResource("/ticket.txt")).mkString
      )

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
          created = new DateTime(2013,6,7,15,35,28,dtz),
          lastUpdated = new DateTime(2013,6,7,16,49,40,dtz),
          starts = None:Option[DateTime],
          started = Some(new DateTime(2013,6,7,16,49,40,dtz)),
          due = Some(new DateTime(2013,6,14,15,35,28,dtz)),
          resolved = None:Option[DateTime],
          told = Some(new DateTime(2013,6,7,16,49,40,dtz))
        ),
        effort = Rt.TicketEffort(
          timeEstimated = 0,
          timeWorked = 0,
          timeLeft = 0
        ),
        customFields = Map(
          Rt.CustomFieldName("Picklist") -> List(
            Rt.CustomFieldValue("This is a picklist"),
            Rt.CustomFieldValue("So it gets formatted like this")
          ),
          Rt.CustomFieldName("Multi Line Customfield") -> List(
            Rt.CustomFieldValue(List(
              "But this is a multiline thing",
              "so it should get the shortest common indent",
              " trimmed off of it."
            ).mkString("\n"))
          )
        )
      )
      //It needs the toStrings for some reason. :(
      ticketDisj.map(_.toString) must_==( \/-(Some(expectedTicket).toString) )

    }
    "Not die if no ticket was found" in {
      val ticketDisj = Ticket.parseTicket(
        dtf,dtz,"RT/4.0.12 200 Ok\n\n# Ticket 198210010 does not exist."
      )

      ticketDisj must_==(\/-(None))
    }
    "Parse some tickets" in {
      val ticketDisj = Ticket.parseTickets(
        dtf,dtz,Source.fromURL(getClass.getResource("/tickets.txt")).mkString
      )

      ticketDisj.map( _.length ) must_==(\/-(3))
      ticketDisj.map( _.map(_.id) ) must_==(\/-(List(1337,1338,1339)))

    }
    "Not die if no tickets were returned" in {
      val ticketDisj = Ticket.parseTickets(
        dtf,dtz,"RT/4.0.12 200 Ok\n\nNo matching results."
      )

      ticketDisj.map( _.length ) must_==(\/-(0))
    }
  }

}
