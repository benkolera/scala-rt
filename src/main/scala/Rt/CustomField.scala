package Rt

import scalaz._
import org.joda.time.DateTime
import Parser.Read

case class CustomFieldName( name: String )
object CustomFieldName {
  def systemName( fn: CustomFieldName ) = s"CF.{${fn.name}}"
}

case class CustomFieldValue( val str: String ) {
  def toInt(): String \/ Int = Read.readInt( str )
  def toDouble(): String \/ Double = Read.readDouble( str )
  def toDateTime(): String \/ DateTime = Read.readDateTime( str )
  def toOptDateTime(): String \/ Option[DateTime] = Read.readOptDateTime( str )
}


object CustomField {
  type Map = scala.collection.immutable.Map[CustomFieldName,CustomFieldValue]
  def tuple( key:String , value:String ) = {
    CustomFieldName(key) -> CustomFieldValue(value)
  }
}
