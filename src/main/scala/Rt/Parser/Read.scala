package com.benkolera.Rt.Parser

import scalaz._
import org.joda.time.format.{DateTimeFormat,DateTimeFormatter}
import org.joda.time.DateTimeZone

object Read {

  // Should turn this into a typeclass. It is pretty clunky.

  def readInt(s:String): String \/ Int = {
    \/.fromTryCatch( s.toInt ).leftMap( _ => s"$s is not an int" )
  }
  def readLong(s:String): String \/ Long = {
    \/.fromTryCatch( s.toLong ).leftMap( _ => s"$s is not an int" )
  }
  def readDouble(s:String): String \/ Double = {
    \/.fromTryCatch( s.toDouble ).leftMap( _ => s"$s is not an int" )
  }
  def readList(s:String): List[String] = {
    s.split(",").toList.map( _.trim )
  }
  def readDateTime(format: DateTimeFormatter, tz: DateTimeZone ) = {
    val tzFormat = format.withZone(tz)
    def read(s:String) = \/.fromTryCatch(
      tzFormat.parseDateTime(s)
    ).leftMap( t => s"$s is not a datetime. Err: ${t.getMessage}" )
    read _
  }

  def readOptDateTime(format: DateTimeFormatter,tz:DateTimeZone)(s:String) =
    s match {
      case ""        => \/-(None)
      case "Not set" => \/-(None)
      case str       => readDateTime(format,tz)(str).map(Some(_))
    }

}
