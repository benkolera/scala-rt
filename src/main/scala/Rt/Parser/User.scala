package com.benkolera.Rt.Parser

import com.benkolera.Rt
import scalaz._
import syntax.applicative._
import syntax.traverse._
import std.option._
import std.list._
import syntax.std.option._
import com.github.nscala_time.time.Imports._
import scala.util.Try
import org.joda.time.format.DateTimeFormatter
import org.joda.time.DateTimeZone
import com.github.tototoshi.csv._
import java.io.StringReader

object User {

  val userIdRe = """^user/(\d+)$""".r
  def extractUserId( s:String ): Parser[Int] = s match {
    case userIdRe(id) => id.toInt.point[Parser]
    case _ => parserFail(InvalidField(
      "id",s"Expected user id to be of form 'user/{id}'. Got: $s"
    ))
  }

  def extractBoolean( string: String ):Parser[Boolean] = \/.right( string == "1" )

  val userEmptyRe = """# No user named (\w+) exists.""".r

  def parseUser(responseStr: String):Parser[Option[Rt.User]] = {
    parseResponse( responseStr ).flatMap{
      case userEmptyRe(id)::ls => none[Rt.User].point[Parser]
      case lines => 
        Field.parseFieldMap( lines ).flatMap( fieldMap => {
          for {
            id <- Field.extractField(fieldMap)("id").flatMap( extractUserId _ )
            n  <- Field.extractField(fieldMap)("Name") 
            e  <- Field.extractField(fieldMap)("EmailAddress").map( some(_).filter(!_.isEmpty) )
            rn <- Field.extractField(fieldMap)("RealName")
            d  <- Field.extractField(fieldMap)("Disabled").flatMap(extractBoolean _ )
          } yield Rt.User.apply(
            id,
            n,
            e,
            rn,
            fieldMap.get("NickName"),
            fieldMap.get("Comments"),
            fieldMap.get("MobilePhone"),
            fieldMap.get("Privileged").fold(false)(_ == "1"),
            d
          ).some
        })
    }
  }

}
