package com.benkolera.Rt.Parser
import scalaz._
import std.list._
import syntax.traverse._
import syntax.monad._
import std.function._
import org.joda.time.{DateTime,DateTimeZone}
import org.joda.time.format.DateTimeFormatter
import scala.util.matching.Regex

case class Field( name: String , value: String )

object Field {

  import Free.Trampoline

  def parseFields( lines: List[String] ):Parser[List[Field]] = {
    import Trampoline.{suspend,done}
    val cleanLines = lines.map(_.stripLineEnd)
    val result = ( cleanLines ++ List("")).zipWithIndex.foldLeft(
      List[Field]().point[FieldParser]
    )(
      (acc,lineWNum) => {
        val (line,lineNum) = lineWNum
        makeFieldParser(
          s => for{
            accState <- suspend( acc.run(s) )
            newState <- suspend( consumeLine(lineNum , line).run(accState._1) )
            out      <- done(
              accState._2.flatMap( accList =>
                newState._2.map( _.map( _ :: accList ).getOrElse( accList ) )
              )
            )
          } yield (newState._1 , out )
        )
      }
    ).run(None).run._2

    EitherT(
      result.bimap(
        e => BadBodyLine( cleanLines , e._2 + 1 , e._1 ), _.reverse
      ).point[Scalaz.Id]
    )

  }

  def parseFieldMap( lines: List[String] ):Parser[Map[String,String]] = {
    parseFields( lines ).map( _.map( f => f.name -> f.value ).toMap )
  }

  def extractField( fieldMap: Map[String,String] )(
    fieldName: String
  ):Parser[String] = {
    fieldMap.get(fieldName).fold[Parser[String]](
      parserFail(MissingField(fieldName))
    )(
      _.point[Parser]
    )
  }

  val extractString = extractField _
  val extractFieldInt = extract( Read.readInt _ ) _
  def extractFieldDateTime(tz:DateTimeZone)(dtf:DateTimeFormatter) =
    extract( Read.readDateTime(dtf,tz) ) _
  def extractFieldOptDateTime(tz:DateTimeZone)(dtf:DateTimeFormatter) =
    extract( Read.readOptDateTime(dtf,tz) ) _

  def extractFieldFields( fieldMap: Map[String,String] )(
    fieldName: String
  ):Parser[List[Field]] = {
    extractField(fieldMap)(fieldName).flatMap( str =>
      parseFields(str.split("\n").toList)
    )
  }

  def extractFieldList( fieldMap: Map[String,String] )(
    fieldName: String
  ):Parser[List[String]] = {
    extractField(fieldMap)(fieldName).map( Read.readList _ )
  }

  def extractOptFieldList( fieldMap: Map[String,String] )(
    fieldName: String
  ):List[String] = {
    fieldMap.get(fieldName).map( Read.readList _ ).getOrElse( Nil )
  }

  def extract[A]( read: (String => String \/ A) )(
    fieldMap: Map[String,String]
  )(
    fieldName: String
  ):Parser[A] = {
    extractField(fieldMap)(fieldName).flatMap( s =>
      EitherT(
        read(s).leftMap( InvalidField( fieldName, _ ) ).point[Scalaz.Id]
      )
    )
  }

  case class PartialField(
    indentRe: Option[Regex],
    name: String,
    value: List[String]
  )
  type FieldParserState[+A] = StateT[Free.Trampoline,Option[PartialField],A]
  type FieldParser[+A] = EitherT[FieldParserState,(String,Int),A]

  //There has got to be a better way to get these types out of the way.
  def makeFieldParser[A](
    s: (Option[PartialField] => Trampoline[(Option[PartialField],(String,Int) \/ A)])
  ) = {
    EitherT[FieldParserState,(String,Int),A]( StateT( s ) )
  }

  def fieldParser[A](s: (Option[PartialField] => (Option[PartialField],A))) =
    makeFieldParser( newState => s(newState).point[Free.Trampoline].map(
      t => t._1 -> \/-(t._2)
    ))


  def getWip = fieldParser( s => (s,s) )
  def endPartialField( successor: Option[PartialField] ) = fieldParser(
    s => {
      (successor,s.map(f => Field(f.name,f.value.reverse.mkString("\n"))))
    }
  )

  def set( pf: Option[PartialField] ) = {
    fieldParser( s => (pf,()) )
  }

  def fail[A]( msg: String , lineNum: Int ) =
    EitherT.left[FieldParserState,(String,Int),A](
      StateT( s => (s,(msg,lineNum)).point[Free.Trampoline] )
    )

  val paddingRe    = """(?s)(\s+).*""".r

  def appendLineToCurrent( lineNum: Int , line: String ):FieldParser[Unit] = {
    getWip.flatMap{
      case None => fail("No field found before response body.",lineNum)
      case Some(pf@PartialField(None,name,_)) => line match {
        case paddingRe(padding) => {
          val maxIndent = Math.min( padding.length, name.length + 2 )
          val re = s"(?s)\\s{${maxIndent}}(.*)".r

          line match {
            case re(rest) => set(
              Some(pf.copy( indentRe = Some(re), value = rest :: pf.value ) )
            )
            case _ => {
              fail(
                s"Start of line didn't match expected indent ($maxIndent): $line",
                lineNum
              )
            }
          }
        }
        case _ => fail(
          s"2nd field line didn't start with a indent.",lineNum
        )
      }
      case Some(pf@PartialField(Some(indentRe),_,_)) => line match {
        case indentRe(rest) => set(
          Some(pf.copy( value = rest :: pf.value ))
        )
        case _ => fail(
          s"Line of a multiline field didn't start with expected indent ($indentRe).",
          lineNum
        )
      }
    }
  }

  def consumeNoop():FieldParser[Option[Field]] = {
    None.point[FieldParser]
  }

  val commentRe     = """^#.+""".r
  val fieldStartRe  = """(?s)(\w.*?): ?(.*)""".r

  def consumeLine( lineNum: Int, line: String ):FieldParser[Option[Field]] = {
    line match {
      case commentRe()             => consumeNoop()
      case fieldStartRe(name,rest) => {
        endPartialField(Some(PartialField(None,name,List(rest))))
      }
      case ""                      => {
        endPartialField(None)
      }
      case _                       => {
        appendLineToCurrent( lineNum, line ).map( _ => None )
      }
    }
  }



}
