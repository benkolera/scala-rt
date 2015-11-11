package com.benkolera.Rt.Parser

import org.specs2._
import scalaz._
import com.github.nscala_time.time.Imports._
import com.benkolera.Rt

object UserParserSpec extends mutable.Specification {

  import scala.io.Source

  "The User Parser" should {
    "Parse a user" in {

      val userDisj = User.parseUser(
        Source.fromURL(getClass.getResource("/user.txt")).mkString
      )

      val expectedUser = Rt.User(
        id = 377964,
        name = "bkolera",
        email = Some("bkolera@iseek.com.au"),
        realName = "Ben Kolera",
        nickName = Some("Ben"),
        comments = None,
        mobilePhone = Some("0488145427"),
        privileged = true,
        disabled = false
      )
      //It needs the toStrings for some reason. :(
      userDisj.map(_.toString) must_==( \/-(Some(expectedUser).toString) )
    }
    "Not die if no user was found" in {
      val userDisj = User.parseUser(
        "RT/4.0.12 200 Ok\n\n# No user named butts exists."
      )
      userDisj must_==(\/-(None))
    }
  }
}
