package com.benkolera.Rt

import org.specs2._
import scalaz._
import org.joda.time.DateTime
import org.joda.time.DateTimeZone


object QueryBuilderSpec extends mutable.Specification {

  import QueryBuilder._

  val dtz = DateTimeZone.UTC
  val dt = new DateTime(2013,7,6,13,33,42,DateTimeZone.forOffsetHours(10))

  val bqs = buildQueryString(dtz) _

  "The query AST" should {

    "Print a simple comparison with an unquoted value" in {
      val s = bqs(Compare( TicketId , Eq , IntValue( 1337 ) ) )
      s must_== ("Id = 1337")
    }
    "Print a simple comparison with an quoted value" in {
      val s = bqs(Compare( TicketId , Eq , StringValue( "1337" )))
      s must_== ("Id = '1337'")
    }
    "Print a simple comparison with a DateTime printed in UTC" in {
      val s = bqs( Compare( Due , Eq , DateTimeValue( dt ) ) )
      s must_== ("Due = '2013-07-06 03:33:42'")
    }
    "Print a conjunction" in {
      val q1 = Compare( TicketId , Eq , IntValue( 1337 ) )
      val q2 = Compare( Queue, Eq , StringValue( "dev.support" ) )
      val q3 = Compare( Status, Eq , StringValue( "open" ) )
      val s = bqs( And(q1,q2,q3) )
      s must_== ("(Id = 1337 AND Queue = 'dev.support' AND Status = 'open')")
    }
    "Print a disjunction" in {
      val q1 = Compare( TicketId , Eq , IntValue( 1337 ) )
      val q2 = Compare( Queue, Eq , StringValue( "dev.support" ) )
      val q3 = Compare( Status, Eq , StringValue( "open" ) )
      val s = bqs( Or(q1,q2,q3) )
      s must_== ("(Id = 1337 OR Queue = 'dev.support' OR Status = 'open')")
    }
    "Print nested queries" in {
      val q1 = Compare( Queue, Eq , StringValue( "dev" ) )
      val q2 = Compare( Status, Eq , StringValue( "open" ) )
      val q3 = Compare( Status, Eq , StringValue( "new" ) )
      val s = bqs( And(q1,Or(q2,q3)) )
      s must_== ("(Queue = 'dev' AND (Status = 'open' OR Status = 'new'))")
    }
    "Print out an 'in' comparison" in {
      val q1 = SetCompare(
        Status, In , NonEmptyList( StringValue("open") , StringValue("new") )
      )
      val s = bqs( q1 )
      s must_== ("(Status = 'open' OR Status = 'new')")
    }
    "Print out an 'not in' comparison" in {
      val q1 = SetCompare(
        Status,NotIn,NonEmptyList(StringValue("closed"),StringValue("rejected"))
      )
      val s = bqs( q1 )
      s must_== ("(Status != 'closed' AND Status != 'rejected')")
    }
  }

  "The query Implicit conversions" should {
    import QueryBuilder._

    "Coerce Strings to StringValues" in {
      val q = Compare( Queue, Eq , "dev" )
      bqs( q ) must_== ("Queue = 'dev'")
    }
    "Coerce CustomFieldNames to CF Identifiers" in {
      val q = Compare( CustomFieldName("Custom"), Eq , "foo" )
      bqs( q ) must_== ("'CF.{Custom}' = 'foo'")
    }
    "Coerce Ints to IntValues" in {
      val q = Compare( TicketId, Eq , 1 )
      bqs( q ) must_== ("Id = 1")
    }
    "Coerce Longs to LongValues" in {
      val q = Compare( TicketId, Eq , 1L )
      bqs( q ) must_== ("Id = 1")
    }
    "Coerce Doubles to DoubleValues" in {
      val q = Compare( TicketId, Eq , 1.0 )
      bqs( q ) must_== ("Id = 1.0")
    }
    "Coerce DateTimes to DateTimeValues" in {
      val q = Compare( Due, Eq , dt )
      bqs( q ) must_== ("Due = '2013-07-06 03:33:42'")
    }
  }
  "The builder syntax" should {
    import QueryBuilder._
    "Build comparisions" in {
      bqs( Due.eqs(dt) ) must_== ("Due = '2013-07-06 03:33:42'")
      bqs(
        CF("Power Level").gt(9000)
      ) must_== ("'CF.{Power Level}' > 9000")
    }
    "Build queries" in {
      val q = Queue.matches("dev") AND CF("Power Level").gt(9000)
      bqs( q ) must_== (
        "(Queue LIKE 'dev' AND 'CF.{Power Level}' > 9000)"
      )
    }
    "Build nested queries like a goddamn champion" in {
      val q = Queue.in("dev","dev.projects") AND (Status.eqs("open") OR Status.eqs("new") )
      bqs( q ) must_== (
        "((Queue = 'dev' OR Queue = 'dev.projects') AND (Status = 'open' OR Status = 'new'))"
      )
    }
    "Build inNel queries fine" in {
      val q = Queue.in("dev","dev.projects") AND Status.inNel(NonEmptyList("open","new"))
      bqs( q ) must_== (
        "((Queue = 'dev' OR Queue = 'dev.projects') AND (Status = 'open' OR Status = 'new'))"
      )
    }

  }

}
