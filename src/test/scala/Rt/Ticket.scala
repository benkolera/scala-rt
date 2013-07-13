package com.benkolera.Rt

import org.specs2._
import org.specs2.mock.Mockito
import org.mockito.Matchers._
import scalaz._
import dispatch._
import com.ning.http.client.Response
import scala.concurrent.{Await,Future}
import scala.concurrent.duration._
import org.joda.time.format.DateTimeFormat

object TicketSpec extends mutable.Specification with Mockito {

  import scalaz.contrib.std.scalaFuture._
  import scala.concurrent.ExecutionContext.Implicits.global
  import scala.concurrent.duration.Duration

  def runMockRtM( http: (Req => Response) , m: RtM[Int] ) = {
    m.run.run(
      Config(
        "username",
        "password",
        "http://rt.test.com",
        DateTimeFormat.forPattern("yyyy-MM-dd HH:mm:ss"),
        scala.concurrent.ExecutionContext.global,
        req => Future( http(req) ).either
      ) ,
      emptyCookieJar
    )
  }

  def mockHttp( method: String, url: String, content: String , res: String )(
    req: Req
  ) = {
    import scala.collection.JavaConversions._
    val r = req.build
    val o = mock[com.ning.http.client.Response]
    val actualContent = for {
      xs <- Option(r.getParams.get("content"))
      x  <- xs.headOption
    } yield x

    if (r.getMethod != method )
      o.getResponseBody() returns "RT/4.0.12 400 Invalid method: " + r.getMethod
    else if (r.getRawUrl != url )
      o.getResponseBody() returns "RT/4.0.12 400 Bad Url: " + r.getRawUrl
    else if (actualContent != Some(content))
      o.getResponseBody() returns s"RT/4.0.12 400 Bad Request: $actualContent"
    else
      o.getResponseBody() returns "RT/4.0.12 200 Ok\n\n" + res

    o
  }

  "The Ticket create function" should {
    "Submit the mandatory params" in {
      val resultFuture = runMockRtM(
        mockHttp(
          "POST",
          "http://rt.test.com/REST/1.0/ticket/new",
          List(
            "id: ticket/new",
            "Subject: test subject",
            "Queue: testQueue",
            "Text: Test text",
            "Requestor: ",
            "Cc: ",
            "AdminCc: ",
            ""
          ).mkString("\n"),
          "# Ticket 1337 created."
        ),
        Ticket.create(NewTicket(
          queue="testQueue",
          subject="test subject",
          text="Test text"
        ))
      )
      val (logs,resultDisj,cj) = Await.result( resultFuture , Duration("5 seconds") )

      if( resultDisj.isLeft ) {
        println( "Failure logs:" )
        logs.foreach( println _ )
        resultDisj must be(Nil)
      }
      resultDisj.isRight must_==(true)
      resultDisj.toOption.get must_==(1337)
    }
  }
}
