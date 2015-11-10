package com.benkolera.Rt.Parser

import org.specs2._
import scalaz._

object FieldParserSpec extends mutable.Specification {
  "The Field Parser" should {
    "Should return no Fields" in {
      Field.parseFields(Nil) must beEqualTo(\/-(Nil))
    }
    "Should return one field for a single line" in {
      Field.parseFields(List(
        "1337: This is a field"
      )) must beEqualTo(\/-(List(Field("1337","This is a field"))))
    }
    "Should handle fields with no value" in {
      Field.parseFields(List(
        "CF.{Cost Centre}:"
      )) must beEqualTo(\/-(List(Field("CF.{Cost Centre}",""))))
    }
    "Should handle fields with curly braces" in {
      Field.parseFields(List(
        "CF.{Progress}: This is progress"
      )) must beEqualTo(\/-(List(Field("CF.{Progress}","This is progress"))))
    }
    "Should return one field for a multiline field" in {
      Field.parseFields(List(
        "1337: This is a field",
        "       That spans over multi lines"
      )) must beEqualTo(\/-(List(
        Field("1337","This is a field\n That spans over multi lines"))
      ))
    }
    "Should return one field for a multiline field that has inconsistent indentation" in {
      Field.parseFields(List(
                          "1337: This is a field",
                          "   That spans over multi lines",
                          "  With some weird assed indentation going on"
                        )) must beEqualTo(\/-(List(
                                                Field("1337","This is a field\n That spans over multi lines\nWith some weird assed indentation going on"))
                                          ))
    }
    "Should return many multiline fields" in {
      Field.parseFields(List(
        "1337: This is a field",
        "      That spans over multi lines",
        "FooBar: bar field",
        "        some more"
      )) must beEqualTo(\/-(List(
        Field("1337","This is a field\nThat spans over multi lines"),
        Field("FooBar","bar field\nsome more")
      )))
    }
    //This is happening for multiline custom fields
    "Should return a multiline field that is indented with 4 spaces" in {
      Field.parseFields(List(
        "CF.{Multi Line Custom Field}: This is a field",
        "    That spans over multi lines",
        "     but isn't indented how everything else is :("
      )) must beEqualTo(\/-(List(
        Field(
          "CF.{Multi Line Custom Field}",
          List(
            "This is a field",
            "That spans over multi lines",
            " but isn't indented how everything else is :("
          ).mkString("\n")
        )
      )))
    }
    "Should error if a field is followed with bad indent" in {
      Field.parseFields(List(
        "Fooo: This is a field",
        "      That spans over multi lines",
        "bad field"
      )) must beEqualTo(-\/(BadBodyLine(
        List("Fooo: This is a field","      That spans over multi lines","bad field"),
        3,
        "Start of line didn't match expected indent (2)"
      )))
    }
    "Should error if the payload doesn't start with a field" in {
      Field.parseFields(List(
        "bad field"
      )) must beEqualTo(-\/(BadBodyLine(
        List("bad field"),
        1,
        "No field found before response body."
      )))
    }
  }
}
