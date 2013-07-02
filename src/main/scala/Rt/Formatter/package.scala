package Rt

import org.joda.time.format.DateTimeFormat
import org.joda.time.DateTimeZone.UTC
import org.joda.time.DateTime
import scalaz._

package object Formatter {
  def fieldListToString( l: List[String] ) = {
    l.mkString(",\n        ")
  }

  private val dtf = DateTimeFormat.forPattern("yyyy-MM-dd HH:mm:ss")

  def fieldDateTimeToString( dt: DateTime ) = {
    dtf.print( dt.withZone( UTC ) )
  }

  def fieldOptDateTimeToString( dt: Option[DateTime] ) = {
    dt.map( fieldDateTimeToString _ ).getOrElse( "Not set" )
  }

  def fieldsToContentString( ts: List[(String,String)] ) = {
    def tupleToString( t:(String,String) ) = {
      val valueEscaped = t._2.replace( "\n" , "\n " )
      s"${t._1}: $valueEscaped\n"
    }
    ts.foldLeft( Cord.empty )( _ ++ tupleToString(_) ).toString
  }
}
