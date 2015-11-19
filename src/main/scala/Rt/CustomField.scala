package com.benkolera.Rt

import scalaz._
import scala.concurrent.Future
import org.joda.time.{DateTime,DateTimeZone}
import org.joda.time.format.{DateTimeFormatter,DateTimeFormat}
import Parser.Read

case class CustomFieldName( name: String )
object CustomFieldName {
  def systemName( fn: CustomFieldName ) = s"CF.{${fn.name}}"
}

case class CustomFieldValue( val string: String ) {
  override def toString: String = string
  def toInt(): String \/ Int = Read.readInt( toString )
  def toDouble(): String \/ Double = Read.readDouble( toString )

  val cfDtf = DateTimeFormat.forPattern("yyyy-MM-dd HH:mm:ss")
  def toDateTime(): String \/ DateTime =
    Read.readDateTime( cfDtf , DateTimeZone.UTC )( toString )
  def toOptDateTime(): String \/ Option[DateTime] =
    Read.readOptDateTime( cfDtf , DateTimeZone.UTC )( toString )
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
