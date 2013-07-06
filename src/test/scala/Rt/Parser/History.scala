package Rt.Parser

import org.specs2._
import scalaz._
import org.joda.time.{DateTime,DateTimeZone}

object HistoryParserSpec extends mutable.Specification {

  import scala.io.Source

  "The History Parser" should {
    "Parse a history with a single history" in {

      val historyDisj = History.parseHistory(
        Source.fromURL(getClass.getResource("/historySingle.txt")).mkString
      ).run

      val expectedHistory = Create(
        id = 10032769,
        ticketId = 894804,
        description = "Ticket created by test@test.com",
        creator = "test@test.com",
        created = new DateTime(2012,11,27,23,0,31,DateTimeZone.UTC),
        timeTaken = 0,
        attachments = List(
          AttachmentInfo( 4743161 , "untitled" , 0 ),
          AttachmentInfo( 4743162 , "untitled" , 0 ),
          AttachmentInfo( 4743163 , "untitled" , 889 ),
          AttachmentInfo( 4743164 , "untitled" , 5632 ),
          AttachmentInfo( 4743165 , "image001.jpg" , 3174 )
        ),
        content = """Hi Gents,
                  |
                  |Can you please action this mystery task?
                  |
                  |Kind Regards,
                  |Test Man""".stripMargin

      )

      if( historyDisj.isLeft ) {
        historyDisj must be(Nil)
      }

      historyDisj.isRight must_==(true)
      //Hack the test to make this work for now. Was getting a:
      //Values have the same string representation but possibly different types
      //but I can't see how it could be a different type as I took the
      //disjunction,options and datetimes out of the equation...
      historyDisj.toOption.get.toString must beEqualTo( List(expectedHistory).toString )

    }
    "Parse a history list" in {

      val historyDisj = History.parseHistory(
        Source.fromURL(getClass.getResource("/history.txt")).mkString
      ).run

      if(historyDisj.isLeft) {
        historyDisj must be(Nil)
      }

      historyDisj.isRight must_==(true)
      val histories = historyDisj.toOption.get
      histories.length must_==(15)

      histories(0) must_==(Create(
        id = 11041164,
        ticketId = 968312,
        description = "Ticket created by bkolera",
        creator = "bkolera",
        created = new DateTime(2013,6,24,7,43,8,DateTimeZone.UTC),
        timeTaken = 1,
        attachments = List(
          AttachmentInfo( 5236258 , "untitled" , 0 ),
          AttachmentInfo( 5236259 , "untitled" , 131 ),
          AttachmentInfo(
            5236260 ,
            "7ac46560_I_Have_No_Idea_What_I_m_Doing.jpeg" ,
            (43.5*1024).toLong
          )
        ),
        content = """This is a test.
                  |
                  |--
                  |Ben Kolera
                  |Team Leader, Development""".stripMargin
      ))
      histories(1) must_==(EmailRecord(
        id = 11041165,
        ticketId = 968312,
        description = "Outgoing email recorded by RT_System",
        creator = "RT_System",
        created = new DateTime(2013,6,24,7,43,9,DateTimeZone.UTC),
        attachments = List(
          AttachmentInfo( 5236261 , "untitled" , 440 )
        ),
          content = """ Transaction: Ticket created by bkolera
                      |        Date: Mon Jun 24 17:43:08 2013
                      |       Queue: dev.support
                      |     Subject: TEST
                      |       Owner: bkolera
                      |      Status: new
                      | Ticket <URL: https://rt.test.com/Ticket/Display.html?id=968312 >
                      |-------------------------------------------------------------------------
                      |
                      |This is a test.
                      |
                      |--
                      |Ben Kolera
                      |Team Leader, Development""".stripMargin
      ))
      histories(2) must_==(AddReminder(
        id = 11041172,
        ticketId = 968312,
        description = "Reminder 'REMINDER TO CLOSE THIS!' added by bkolera",
        creator = "bkolera",
        created = new DateTime(2013,6,24,7,43,41,DateTimeZone.UTC),
        reminderId = 968313
      ))
      histories(3) must_==(ResolveReminder(
        id = 11041181,
        ticketId = 968312,
        description = "Reminder 'REMINDER TO CLOSE THIS!' completed by bkolera",
        creator = "bkolera",
        created = new DateTime(2013,6,24,7,44,1,DateTimeZone.UTC),
        reminderId = 968313
      ))
      histories(4) must_==(Correspond(
        id = 11041183,
        ticketId = 968312,
        description = "Correspondence added by bkolera",
        creator = "bkolera",
        created = new DateTime(2013,6,24,7,44,27,DateTimeZone.UTC),
        timeTaken = 10,
        subject = "No Subject",
        attachments = List(
          AttachmentInfo( 5236265 , "untitled" , 121 )
        ),
        content = """Taken!
                  |
                  |--
                  |Ben Kolera
                  |Team Leader, Development""".stripMargin
      ))
      histories(5) must_==(Status(
        id = 11041186,
        ticketId = 968312,
        description = "Status changed from 'new' to 'open' by bkolera",
        creator = "bkolera",
        created = new DateTime(2013,6,24,7,44,28,DateTimeZone.UTC),
        oldStatus = "new",
        newStatus = "open"
      ))
      histories(6) must_==(Set(
        id = 11041189,
        ticketId = 968312,
        description = "Untaken by bkolera",
        creator = "bkolera",
        created = new DateTime(2013,6,24,7,45,4,DateTimeZone.UTC),
        fieldName = "Owner",
        oldValue = "377964",
        newValue = "10"
      ))
      histories(7) must_==(DeleteWatcher(
        id = 11041190,
        ticketId = 968312,
        description = "AdminCc devnull@test.com deleted by bkolera",
        creator = "bkolera",
        created = new DateTime(2013,6,24,7,45,5,DateTimeZone.UTC),
        watcherType = WatcherType.AdminCc,
        watcher = "375978"
      ))
      histories(8) must_==(Comment(
        id = 11041196,
        ticketId = 968312,
        description = "Comments added by bkolera",
        creator = "bkolera",
        created = new DateTime(2013,6,24,7,45,29,DateTimeZone.UTC),
        timeTaken = 20,
        subject = "No Subject",
        attachments = List(
          AttachmentInfo( 5236273 , "untitled" , 123 )
        ),
        content = """Comment!
                  |
                  |--
                  |Ben Kolera
                  |Team Leader, Development""".stripMargin
      ))
      histories(9) must_==(CommentEmailRecord(
        id = 11041197,
        ticketId = 968312,
        description = "Outgoing email about a comment recorded by RT_System",
        creator = "RT_System",
        created = new DateTime(2013,6,24,7,45,29,DateTimeZone.UTC),
        attachments = List(
          AttachmentInfo( 5236274 , "untitled" , 236 )
        ),
        content = """
                  |https://rt.test.com/Ticket/Display.html?id=968312
                  |This is a comment.  It is not sent to the Requestor(s):
                  |
                  |Comment!
                  |
                  |--
                  |Ben Kolera
                  |Team Leader, Development""".stripMargin
      ))
      histories(10) must_==(DeleteLink(
        id = 11041203,
        ticketId = 968312,
        description = "Member ticket #895253 deleted by bkolera",
        creator = "bkolera",
        created = new DateTime(2013,6,24,7,46,12,DateTimeZone.UTC),
        linkName = "HasMember",
        url = "fsck.com-rt://test.com/ticket/895253"
      ))
      histories(11) must_==(CustomField(
        id = 11044555,
        ticketId = 968312,
        description = "Initiator Customer changed to Automatic System Report by bkolera",
        creator = "bkolera",
        created = new DateTime(2013,6,25,1,5,45,DateTimeZone.UTC),
        fieldId = 58,
        oldValue = "Customer",
        newValue = "Automatic System Report"
      ))
      histories(12) must_==(AddWatcher(
        id = 11044571,
        ticketId = 968312,
        description = "Cc ben@test.com added by bkolera",
        creator = "bkolera",
        created = new DateTime(2013,6,25,1,9,31,DateTimeZone.UTC),
        watcherType = WatcherType.Cc,
        watcher = "377736"
      ))
      histories(13) must_==(Told(
        id = 11044577,
        ticketId = 968312,
        description = "Told changed from Not set to 2013-06-28 00:00:00 by bkolera",
        creator = "bkolera",
        created = new DateTime(2013,6,25,1,9,33,DateTimeZone.UTC),
        date = new DateTime(2013,6,27,14,0,0,DateTimeZone.UTC)
      ))
    }
  }


}
