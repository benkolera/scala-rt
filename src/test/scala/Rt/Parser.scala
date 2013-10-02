package com.benkolera.Rt.Parser

import org.specs2._
import scalaz._

object ParserSpec extends mutable.Specification {
  "The response parser" should {
    "Throw an error on an empty array" in {
      parseResponse("").run must beEqualTo(-\/(BadResponse("")))
    }
    "Throw an error on the 0 response" in {
      parseResponse("0").run must beEqualTo(-\/(BadResponse("0")))
    }
    "Throw an BadResponse error on non 200 Response" in {
      parseResponse(
        """RT/4.0.14 500 Internal Server Error
          |""".stripMargin
      ).run must beEqualTo(-\/(BadResponse(
        """RT/4.0.14 500 Internal Server Error""".stripMargin
      )))
    }
    "Throw an CredentialsRequired for 401" in {
      parseResponse(
        """RT/4.0.14 401 Credentials required""".stripMargin
      ).run must beEqualTo(-\/(CredentialsRequired))
    }
    "Return the remaining lines on a 200 Ok" in {
      parseResponse(
        """RT/4.0.12 200 Ok
          |
          |948619: TADSL - Query regarding SQ results and possible WG release update - 0755730637
          |956459: FW: No network service 8GB on a 12GB Value Cap
          |""".stripMargin
      ).run must beEqualTo(\/-(List(
        "948619: TADSL - Query regarding SQ results and possible WG release update - 0755730637",
        "956459: FW: No network service 8GB on a 12GB Value Cap"
      )))
    }
    "Return empty list for an empty 200 ok" in {
      parseResponse(
        """RT/4.0.12 200 Ok
          |
          |""".stripMargin
      ).run must beEqualTo(\/-(Nil))
    }
  }
}
