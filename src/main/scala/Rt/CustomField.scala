package com.benkolera.Rt

import scalaz._
import scala.concurrent.Future
import org.joda.time.{DateTime,DateTimeZone}
import org.joda.time.format.DateTimeFormatter
import Parser.Read

case class CustomFieldName( name: String )
object CustomFieldName {
  def systemName( fn: CustomFieldName ) = s"CF.{${fn.name}}"
}

case class CustomFieldValue( val string: String ) {
  override def toString: String = string
  def toInt(): String \/ Int = Read.readInt( toString )
  def toDouble(): String \/ Double = Read.readDouble( toString )
  def toDateTime(dtf:DateTimeFormatter,tz:DateTimeZone): String \/ DateTime =
    Read.readDateTime( dtf , tz )( toString )
  def toDateTimeRtM(implicit m:Monad[Future]): RtM[String \/ DateTime] = {
    getConfig.map( c =>
      toDateTime(c.dateTimeFormatter,c.dateTimeZone)
    )
  }
  def toOptDateTime(
    dtf:DateTimeFormatter,tz:DateTimeZone
  ): String \/ Option[DateTime] =
    Read.readOptDateTime( dtf ,tz )( toString )

  def toOptDateTimeRtM(implicit m:Monad[Future]): RtM[String \/ Option[DateTime]] = {
    getConfig.map( c =>
      toOptDateTime(c.dateTimeFormatter,c.dateTimeZone)
    )
  }
}

object CustomFieldValue {
  def fromInt(i:Int) = CustomFieldValue(i.toString)
  def fromDouble(d:Double) = CustomFieldValue(d.toString)
  def fromDateTime(dt:DateTime,dtf:DateTimeFormatter) =
    fromOptDateTime(Some(dt),dtf)
  def fromOptDateTime( dt:Option[DateTime],dtf:DateTimeFormatter ) =
    CustomFieldValue(dt.map( dtf.print _ ).getOrElse( "Not set" ))
}


object CustomField {
  type Map = scala.collection.immutable.Map[CustomFieldName,List[CustomFieldValue]]
  def tuple( key:String , value:String ) = {
    CustomFieldName(key) -> List(CustomFieldValue(value))
  }
}
