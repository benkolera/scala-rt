package com.benkolera.Rt

import org.joda.time.format.DateTimeFormat
import org.joda.time.{DateTimeZone,DateTime}
import scalaz._

package object Formatter {
  def fieldListToString( l: List[String] ) = {
    l.mkString(",\n        ")
  }

  private val dtf = DateTimeFormat.forPattern("yyyy-MM-dd HH:mm:ss")

  def fieldDateTimeToString(tz:DateTimeZone )(dt: DateTime) = {
    dtf.print( dt.withZone( tz ) )
  }

  def fieldOptDateTimeToString( tz:DateTimeZone )( dt: Option[DateTime] ) = {
    dt.map( fieldDateTimeToString(tz) _ ).getOrElse( "Not set" )
  }

  def fieldsToContentString( ts: List[(String,String)] ) = {
    def tupleToString( t:(String,String) ) = {
      val valueEscaped = t._2.replace( "\n" , "\n " )
      s"${t._1}: $valueEscaped\n"
    }
    ts.foldLeft( Cord.empty )( _ ++ tupleToString(_) ).toString
  }
}
