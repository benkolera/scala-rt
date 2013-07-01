package Rt.Parser
import scalaz._
import std.list._
import syntax.traverse._
import syntax.monad._
import org.joda.time.DateTime
import org.joda.time.format.DateTimeFormat
import org.joda.time.DateTimeZone.UTC

case class Field( name: String , value: String )

object Field {

  def parseFields( lines: List[String] ):ParserError \/ List[Field] = {
    (lines ++ List("")).zipWithIndex.foldLeft[FieldParser[List[Field]]](
      Nil.point[FieldParser]
    )(
      (acc,lineWNum) => acc.flatMap( outList =>
        consumeLine( lineWNum._2 , lineWNum._1 ).map( _  match {
          case None    => outList
          case Some(f) => f :: outList
        } )
      )
    ).run(None)._2.bimap(
      err => BadBodyLine( lines , err._2 + 1 , err._1 ),
      _.reverse
    )
  }

  def parseFieldMap( lines: List[String] ):ParserError \/ Map[String,String] = {
    parseFields( lines ).map( _.map( f => f.name -> f.value ).toMap )
  }

  def extractField( fieldMap: Map[String,String] )(
    fieldName: String
  ):ParserError \/ String = {
    fieldMap.get(fieldName).fold[ParserError \/ String](
      -\/(MissingField(fieldName))
    )(
      \/-(_)
    )
  }

  def extractFieldInt( fieldMap: Map[String,String] )(
    fieldName: String
  ):ParserError \/ Int = {
    extractField(fieldMap)(fieldName).flatMap(
      Read.readInt(_).leftMap( InvalidField( fieldName, _ ) )
    )
  }

  def extractFieldDateTime( fieldMap: Map[String,String] )(
    fieldName: String
  ):ParserError \/ DateTime = {
    extractField(fieldMap)(fieldName).flatMap(
      Read.readDateTime(_).leftMap( InvalidField( fieldName, _ ) )
    )
  }

  def extractFieldOptDateTime( fieldMap: Map[String,String] )(
    fieldName: String
  ):ParserError \/ Option[DateTime] = {
    extractField(fieldMap)(fieldName).flatMap{
      Read.readOptDateTime(_).leftMap(InvalidField( fieldName, _ ))
    }
  }

  def extractFieldFields( fieldMap: Map[String,String] )(
    fieldName: String
  ):ParserError \/ List[Field] = {
    extractField(fieldMap)(fieldName).flatMap( str =>
      parseFields(str.split("\n").toList)
    )
  }

  def extractFieldList( fieldMap: Map[String,String] )(
    fieldName: String
  ):ParserError \/ List[String] = {
    extractField(fieldMap)(fieldName).map( Read.readList _ )
  }

  case class PartialField( indent: Int, name: String, value: List[String])
  type FieldParserState[+A] = State[Option[PartialField],A]
  type FieldParser[+A] = EitherT[FieldParserState,(String,Int),A]

  def fieldParser[A](s: (Option[PartialField] => (Option[PartialField],A))) =
    EitherT.right[FieldParserState,(String,Int),A]( State( s ) )

  def getWip = fieldParser( s => (s,s) )
  def endPartialField( successor: Option[PartialField] ) = fieldParser(
    s => {
      (successor,s.map(f => Field(f.name,f.value.reverse.mkString("\n"))))
    }
  )

  def fail[A]( msg: String , lineNum: Int ) =
    EitherT.left[FieldParserState,(String,Int),A](
      State( s => (s,(msg,lineNum)) )
    )

  val paddingRe    = """^\s+$""".r

  def appendLineToCurrent( lineNum: Int , line: String ):FieldParser[Unit] = {
    getWip.flatMap{
      case None => fail("No field found before response body.",lineNum)
      case Some(p) => {
        val (padding,value) = line.splitAt(p.indent)
        padding match {
          case paddingRe() => fieldParser(
            s => (Some(p.copy( value = value :: p.value )),())
          )
          case _  =>  fail("Line didn't start with a field or indent.",lineNum)
        }
      }
    }
  }

  def consumeNoop():FieldParser[Option[Field]] = {
    None.point[FieldParser]
  }

  val commentRe     = """^#.+""".r
  val fieldStartRe  = """(\w.*?): ?(.*)""".r

  def consumeLine( lineNum: Int, line: String ):FieldParser[Option[Field]] = {
    line match {
      case commentRe()             => consumeNoop()
      case fieldStartRe(name,rest) => endPartialField(Some(PartialField(name.size + 2,name,List(rest))))
      case ""                      => endPartialField(None)
      case _                       => appendLineToCurrent( lineNum, line ).map( _ => None )
    }
  }



}
