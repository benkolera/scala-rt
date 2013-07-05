package Rt

import org.joda.time.DateTime

case class QueryResult( ticketId: Int , subject: String )

object Query {

  import dispatch._
  import scalaz._
  import syntax.monad._
  import scalaz.contrib.std.scalaFuture._
  import std.list._
  import std.either._
  import scala.concurrent.Future
  import scala.collection.JavaConversions._
  import scala.util.matching.Regex

  def query(query:String,orderBy:Option[String])(
    implicit m:Monad[Future]
  ) = {
    val queryMap = ("query",query) ::
      orderBy.map( o => List("orderBy" -> o) ).getOrElse(Nil)

    for {
      req  <- rtApi.map( _ / "search" / "ticket" << queryMap )
      body <- callApi( req )
      res  <- liftParseError(Parser.Query.parseQueryResponse(body))
    } yield res
  }

}
