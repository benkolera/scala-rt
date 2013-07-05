package Rt.Parser
import scalaz._

object Query {
  def parseQueryResponse( responseStr: String ) = {
    parseResponse( responseStr.split("\n").toList ).flatMap( lines =>
      Field.parseFields( lines ).map(
        _.map( f => Rt.QueryResult( f.name.toInt , f.value ) )
      )
    )
  }



}
