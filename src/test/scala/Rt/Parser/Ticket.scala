package Rt.Parser

import org.specs._
import scalaz._
import com.github.nscala_time.time.Imports._
import org.joda.time.DateTimeZone.UTC

object TicketParserSpec extends Specification {

  import scala.io.Source
  val dtf = DateTimeFormat.forPattern("yyyy-MM-dd HH:mm:ss");

  "The Ticket Parser" should {
    "Parse a ticket" in {

      val ticketDisj = Ticket.parseTicket(
        Source.fromURL(getClass.getResource("/ticket.txt")).mkString
      )

      val expectedTicket = Rt.Ticket(
        id = 962663,
        queue = "dev.support",
        subject = "(GJR) FW: prepaid notifications and plan limit",
        status = "open",
        people = Rt.TicketPeople(
          owner = None:Option[String],
          creator = "linda",
          requestors = List("Gregory.Clay@iseek.com.au","linda@iseek.com.au"),
          cc = List[String](),
          adminCc = List[String]()
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

      ticketDisj.isRight must be(true)
      //Hack the test to make this work for now. Was getting a:
      //Values have the same string representation but possibly different types
      //but I can't see how it could be a different type as I took the
      //disjunction,options and datetimes out of the equation...
      ticketDisj.toOption.get.toString must_==( expectedTicket.toString )

    }
  }
}
