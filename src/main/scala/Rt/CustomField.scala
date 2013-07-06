package Rt

import scalaz._
import org.joda.time.DateTime
import org.joda.time.format.DateTimeFormatter
import Parser.Read

case class CustomFieldName( name: String )
object CustomFieldName {
  def systemName( fn: CustomFieldName ) = s"CF.{${fn.name}}"
}

case class CustomFieldValue( val str: String ) {
  def toInt(): String \/ Int = Read.readInt( str )
  def toDouble(): String \/ Double = Read.readDouble( str )
  def toDateTime(dtf:DateTimeFormatter): String \/ DateTime =
    Read.readDateTime( dtf )( str )
  def toOptDateTime(dtf:DateTimeFormatter): String \/ Option[DateTime] =
    Read.readOptDateTime( dtf )( str )
}


object CustomField {
  type Map = scala.collection.immutable.Map[CustomFieldName,CustomFieldValue]
  def tuple( key:String , value:String ) = {
    CustomFieldName(key) -> CustomFieldValue(value)
  }
}
