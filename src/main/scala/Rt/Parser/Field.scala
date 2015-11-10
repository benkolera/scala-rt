package com.benkolera.Rt.Parser
import scalaz._
import std.list._
import syntax.traverse._
import syntax.monad._
import std.function._
import std.anyVal._
import syntax.std.boolean._
import std.option._
import syntax.std.option._
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

    result.bimap(
      e => BadBodyLine( cleanLines , e._2 + 1 , e._1 ), _.reverse
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
      read(s).leftMap( InvalidField( fieldName, _ ) )
    )
  }

  case class PartialField(
    name: String,
    value: NonEmptyList[(Int,String)]
  )
  type FieldParserState[A] = StateT[Free.Trampoline,Option[PartialField],A]
  type FieldParser[A] = EitherT[FieldParserState,(String,Int),A]

  //There has got to be a better way to get these types out of the way.
  def makeFieldParser[A](
    s: (Option[PartialField] => Trampoline[(Option[PartialField],(String,Int) \/ A)])
  ) = {
    EitherT[FieldParserState,(String,Int),A]( StateT( s ) )
  }

  def fieldParser[A](s: (Option[PartialField] => (Option[PartialField],A))):FieldParser[A] =
    makeFieldParser( newState => s(newState).point[Free.Trampoline].map(
      t => t._1 -> \/-(t._2)
    ))


  def getWip:FieldParser[Option[PartialField]] = fieldParser( s => (s,s) )

  def validateIndentation( indent:Int, indentationRe:Regex )( line:(Int,String) ):Validation[NonEmptyList[(String,Int)],String] = {
    line._2 match {
      case indentationRe(rest) => Validation.success[NonEmptyList[(String,Int)],String]( rest )
      case _ => Validation.failure[NonEmptyList[(String,Int)],String](NonEmptyList((
        s"Start of line didn't match expected indent ($indent)",
        line._1
      )))
    }
  }

  def endPartialField( successor: Option[PartialField] ) = makeFieldParser[Option[Field]]( s =>
    Trampoline.delay((successor,s.fold( \/.right[(String,Int),Option[Field]](none[Field]) ){ pf =>
      val outList   = pf.value.reverse
      outList.tail match {
        case Nil   => \/.right( Field( pf.name , outList.head._2 ).some )
        case rest  => {
          val maxIndent = rest.map( _._2.takeWhile( _ == ' ' ).length ).sorted.headOption.getOrElse(0)
          val minIndent = Math.max( 2, Math.min( maxIndent, pf.name.length + 2 ) )

          val re = s"(?s)\\s{${minIndent}}(.*)".r

          outList.tail.traverseU( validateIndentation( minIndent, re ) _ ).map(
            x => outList.head._2 + (x.empty).fold("","\n" + x.mkString("\n"))
          ) match {
            case Success(s) => \/.right(some(Field(pf.name,s)))
            case Failure(f) => \/.left( f.head )
          }
        }
      }
    }))
  )

  def set( pf: Option[PartialField] ):FieldParser[Unit] = {
    fieldParser[Unit]( s => (pf,()) )
  }

  def fail[A]( msg: String , lineNum: Int ) =
    EitherT.left[FieldParserState,(String,Int),A](
      StateT( s => (s,(msg,lineNum)).point[Free.Trampoline] )
    )

  val paddingRe    = """(?s)(\s+).*""".r

  def appendLineToCurrent( lineNum: Int , line: String ):FieldParser[Unit] = {
    getWip.flatMap{
      case None => fail[Unit]("No field found before response body.",lineNum)
      case Some(pf) => set( pf.copy( value = (lineNum,line) <:: pf.value ).some )
    }
  }

  def consumeNoop():FieldParser[Option[Field]] = {
    none[Field].point[FieldParser]
  }

  val commentRe     = """^#.+""".r
  val fieldStartRe  = """(?s)(\w.*?): ?(.*)""".r

  def consumeLine( lineNum: Int, line: String ):FieldParser[Option[Field]] = {
    line match {
      case commentRe()             => consumeNoop()
      case fieldStartRe(name,rest) => {
        endPartialField(Some(PartialField(name,NonEmptyList(lineNum -> rest))))
      }
      case ""                      => {
        endPartialField(none[PartialField])
      }
      case _                       => {
        appendLineToCurrent( lineNum, line ).map( _ => none[Field] )
      }
    }
  }



}
