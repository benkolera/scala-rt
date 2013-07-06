package Rt

import org.joda.time.DateTime
import scalaz._
import org.joda.time.format.DateTimeFormat
import org.joda.time.DateTimeZone.UTC

object QueryBuilder {
  sealed trait Query {
    def AND ( q2: Query ) = And( this , q2 )
    def OR  ( q2: Query ) = Or( this , q2 )
  }
  case class And( e1: Query , e2: Query , rest: Query* ) extends Query
  case class Or( e1: Query , e2: Query , rest: Query* ) extends Query
  case class Compare( id: Identifier , c: Comparator , v: Value ) extends Query

  sealed trait Identifier {
    def gt( v:Value ) = Compare( this , Gt , v )
    def ln( v:Value ) = Compare( this , Lt , v )
    def eqs( v:Value ) = Compare( this , Eq , v )
    def neq( v:Value ) = Compare( this , Ne , v )
    def matches( v:StringValue ) = Compare( this , Matches , v )
    def notMatches( v:StringValue ) = Compare( this , DoesntMatch , v )
  }
  case object TicketId extends Identifier
  case object Queue extends Identifier
  case object Status extends Identifier
  case object Subject extends Identifier
  case object Owner extends Identifier
  case object Creator extends Identifier
  case object LastUpdatedBy extends Identifier
  case object Created extends Identifier
  case object Due extends Identifier
  case object Starts extends Identifier
  case object Started extends Identifier
  case object LastContacted extends Identifier
  case object LastUpdated extends Identifier
  case object Updated extends Identifier
  case object TimeWorked extends Identifier
  case object TimeEstimated extends Identifier
  case object TimeLeft extends Identifier
  case class CF( name: String ) extends Identifier

  sealed trait Value
  case class StringValue( s:String ) extends Value
  case class IntValue( i:Int ) extends Value
  case class LongValue( l:Long ) extends Value
  case class DoubleValue( d:Double ) extends Value
  case class DateTimeValue( dt:DateTime ) extends Value

  sealed trait Comparator
  case object Eq extends Comparator
  case object Ne extends Comparator
  case object Gt extends Comparator
  case object Lt extends Comparator
  case object Matches extends Comparator
  case object DoesntMatch extends Comparator

  sealed trait OrderBy
  case class Asc( id: Identifier ) extends OrderBy
  case class Desc( id: Identifier ) extends OrderBy

  object Implicits {
    import language.implicitConversions

    implicit def stringToValue( s: String ) = StringValue(s)
    implicit def intToValue( i: Int ) = IntValue(i)
    implicit def longToValue( l: Long ) = LongValue(l)
    implicit def doubleToValue( d: Double ) = DoubleValue(d)
    implicit def dateTimeToValue( dt: DateTime ) = DateTimeValue(dt)
  }

  val dtf = DateTimeFormat.forPattern( "yyyy-MM-dd HH:mm:ss" )

  def buildQueryString( q:Query ) = buildQueryCord(q).toString
  def buildOrderByString( ob:OrderBy ) = ob match {
    case Asc(id)  => idCord(id).toString
    case Desc(id) => (Cord("-") ++ idCord(id)).toString
  }

  // This isn't tail recursive, but this should be OK
  def buildQueryCord( q:Query ):Cord = q match {
    case And(e1,e2,er @_*) => expListCord( "AND",e1,e2,er:_* )
    case Or(e1,e2,er @_*)  => expListCord( "OR" ,e1,e2,er:_* )
    case Compare(id,cmp,v) => Cord.mkCord(
      Cord(" "), idCord(id) , cmpCord(cmp) , valueCord(v)
    )
  }

  def expListCord( sep:String, e1: Query, e2:Query, er: Query* ) = {
    val spacedSep = Cord.fromStrings( Seq(" " , sep , " ") )
    Cord("(") ++
    Cord.mkCord( spacedSep, (e1::e2::er.toList).map(buildQueryCord _) :_* ) ++
    Cord(")")
  }

  def idCord( id:Identifier ) = Cord(id match {
    case CF( name ) => s"CF.{$name}"
    case TicketId   => "Id"
    case _          => id.toString //Probably going to regret this...
  })

  def cmpCord( cmp:Comparator ) = Cord(cmp match {
    case Eq          => "="
    case Ne          => "!="
    case Gt          => ">"
    case Lt          => "<"
    case Matches     => "LIKE"
    case DoesntMatch => "NOT LIKE"
  })

  def valueCord( v:Value ) = v match {
    case StringValue(s)    => quote(s)
    case DoubleValue(d)    => Cord.fromStrings(Seq(d.toString))
    case LongValue(l)      => Cord.fromStrings(Seq(l.toString))
    case IntValue(i)       => Cord.fromStrings(Seq(i.toString))
    case DateTimeValue(dt) => quote(dtf.print( dt.withZone( UTC ) ))
  }

  def quote( s:String ) = Cord.fromStrings( Seq( "'" , s , "'" ) )

}
