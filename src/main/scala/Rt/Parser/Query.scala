package com.benkolera.Rt.Parser

import com.benkolera.Rt
import scalaz._
import syntax.applicative._

object Query {
  val ticketsEmptyRe = """No matching results.""".r
  def parseQueryResponse( responseStr: String ) = {
    parseResponse( responseStr.split("\n").toList ).flatMap{
      case ticketsEmptyRe()::ls => Nil.point[Parser]
      case lines => Field.parseFields( lines ).map(
        _.map( f => Rt.QueryResult( f.name.toInt , f.value ) )
      )
    }
  }
}
