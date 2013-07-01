package Rt.Parser

import scalaz._
import org.joda.time.format.{DateTimeFormat,DateTimeFormatter}
import org.joda.time.DateTimeZone.UTC

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
  def readDateTimeFormat(format: DateTimeFormatter) = {
    val utcFormat = format.withZone(UTC)
    def read(s:String) = \/.fromTryCatch(
      utcFormat.parseDateTime(s)
    ).leftMap( t => s"$s is not a datetime. Err: ${t.getMessage}" )
    read _
  }

  def readOptDateTimeFormat(format: DateTimeFormatter)(s:String) = s match {
    case ""        => \/-(None)
    case "Not set" => \/-(None)
    case str       => readDateTimeFormat(format)(str).map(Some(_))
  }

  val dtf = DateTimeFormat.forPattern("yyyy-MM-dd HH:mm:ss")
  val readDateTime = readDateTimeFormat(dtf)
  val readOptDateTime = readOptDateTimeFormat(dtf) _

}
