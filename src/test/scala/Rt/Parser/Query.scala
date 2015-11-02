package com.benkolera.Rt.Parser

import com.benkolera.Rt.QueryResult
import org.specs2._
import scalaz._

object QueryParserSpec extends mutable.Specification {
  "The Query Response Parser" should {
    "Return Credentials required for 401" in {
      Query.parseQueryResponse(
        "RT/4.0.14 401 Credentials required"
      ) must beEqualTo(-\/(CredentialsRequired))
    }
    "Return empty list for no Results" in {
      Query.parseQueryResponse(
        """RT/4.0.14 200 Ok
        |
        |No matching results.
        |""".stripMargin
      ) must beEqualTo(\/-(Nil))
    }
    "Return list of query results" in {
      Query.parseQueryResponse(
        """RT/4.0.14 200 Ok
        |
        |948619: TADSL - Query regarding SQ results and possible WG release update - 0755730637
        |956459: FW: No network service 8GB on a 12GB Value Cap
        |""".stripMargin
      ) must beEqualTo(\/-(List(
        QueryResult(948619,"TADSL - Query regarding SQ results and possible WG release update - 0755730637"),
        QueryResult(956459,"FW: No network service 8GB on a 12GB Value Cap")
      )))
    }
    "Not stack overflow" in {
      try {
        Query.parseQueryResponse(
          (
            "RT/4.0.14 200 Ok" ::
              "" ::
              (1 to 10000).map( x => s"$x: Ticket number $x" ).toList
          ).mkString("\n")
        ).map( _.length ) must_==(\/-(10000))
      } catch {
        case e:Throwable => e.printStackTrace() ; throw e
      }
    }
  }
}
