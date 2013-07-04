package Rt.Parser

import scalaz._
import syntax.monad._

object Link {


  val idRe = """ticket/(\d+)/links""".r
  val internalTicketRe = """fsck.com-rt://[\w\.-_]+/ticket/(\d+)""".r

  def extractId( s:String ) = s match {
    case idRe(id) => id.toInt.point[Parser]
    case _        => parserFail(InvalidField("id" , "Couldn't parse ticket id"))
  }

  def extractLink( s: String ): Rt.Link = s match {
    case internalTicketRe(id) => Rt.RtTicketLink(id.toInt)
    case url                  => Rt.ExtUrlLink(url)
  }

  def parseShow( responseStr: String ) = {
    val extOptList = Field.extractOptFieldList _
    val extStr  = Field.extractField _
    for {
      lines <- parseResponse( responseStr.split("\n").toList )
      fs    <- Field.parseFieldMap( lines )
      id    <- extStr(fs)("id").flatMap(extractId)
    } yield  Rt.TicketLinks(
      ticketId  = id,
      dependsOn = extOptList(fs)("DependsOn").map(extractLink _ ),
      dependedOnBy = extOptList(fs)("DependedOnBy").map(extractLink _ ),
      refersTo = extOptList(fs)("RefersTo").map(extractLink _ ),
      referredToBy = extOptList(fs)("ReferredToBy").map(extractLink _ ),
      members = extOptList(fs)("Members").map( extractLink _ ),
      memberOf = fs.get("MemberOf").map(extractLink _)
    )
  }



}
