package Rt.Parser
import scalaz._
import std.list._
import syntax.traverse._
import syntax.monad._

case class Field( name: String , value: String )

object Field {
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

  val fieldStartRe    = """([\w\.\{\}]+): (.+)""".r

  def consumeLine( lineNum: Int, line: String ):FieldParser[Option[Field]] = {
    line match {
      case fieldStartRe(name,rest) => endPartialField(Some(PartialField(name.size + 2,name,List(rest))))
      case ""                      => endPartialField(None)
      case _                       => appendLineToCurrent( lineNum, line ).map( _ => None )
    }
  }

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

}
