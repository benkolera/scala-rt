package com.benkolera.Rt

import org.specs2._
import scalaz._

object PaginatedResultsSpec extends mutable.Specification {
  import PaginatedResults._
  import scalaz.contrib.std.scalaFuture._
  import scala.concurrent.ExecutionContext.Implicits.global

  "The page count calculator" should {
    "Throw an error on a width < 0" in {
      calculatePageCount(10,0) must beEqualTo(
        -\/(InvalidPagination("Page width should be greater than 0"))
      )
    }
    "Return 1 page for count <= width" in {
      calculatePageCount(0,10) must beEqualTo(\/-(1))
      calculatePageCount(1,10) must beEqualTo(\/-(1))
      calculatePageCount(10,10) must beEqualTo(\/-(1))
    }
    "Return more than 1 page for count > width" in {
      calculatePageCount(11,10) must beEqualTo(\/-(2))
      calculatePageCount(20,10) must beEqualTo(\/-(2))
      calculatePageCount(21,10) must beEqualTo(\/-(3))
      calculatePageCount(1000,20) must beEqualTo(\/-(50))
    }
  }
}
