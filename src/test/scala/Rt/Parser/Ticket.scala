package Rt.Parser

import org.specs2._
import scalaz._
import com.github.nscala_time.time.Imports._
import org.joda.time.DateTimeZone.UTC

object TicketParserSpec extends mutable.Specification {

  import scala.io.Source
  val dtf = DateTimeFormat.forPattern("yyyy-MM-dd HH:mm:ss");

  "The Ticket Parser" should {
    "Parse a ticket" in {

      val ticketDisj = Ticket.parseTicket(
        Source.fromURL(getClass.getResource("/ticket.txt")).mkString
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
          Rt.CustomField.tuple("Cost Centre",""),
          Rt.CustomField.tuple("Product",""),
          Rt.CustomField.tuple("Development Work Type",""),
          Rt.CustomField.tuple("Initiator",""),
          Rt.CustomField.tuple("Progress","")
        )
      )
      //It needs the toStrings for some reason. :(
      ticketDisj.map(_.toString) must_==( \/-(expectedTicket.toString) )

    }
    "Parse some tickets" in {

      val ticketDisj = Ticket.parseTickets(
        Source.fromURL(getClass.getResource("/tickets.txt")).mkString
      ).run

      ticketDisj.map( _.length ) must_==(\/-(3))
      ticketDisj.map( _.map(_.id) ) must_==(\/-(List(1337,1338,1339)))

    }
  }

}
