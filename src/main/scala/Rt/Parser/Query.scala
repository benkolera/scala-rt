package Rt.Parser
import scalaz._

case class QueryResult( ticketId: Int , subject: String )

object Query {
  def parseQueryResponse( responseStr: String ) = {
    parseResponse( responseStr.split("\n").toList ).flatMap( lines =>
      Field.parseFields( lines ).map(
        _.map( f => QueryResult( f.name.toInt , f.value ) )
      )
    )
  }



}
