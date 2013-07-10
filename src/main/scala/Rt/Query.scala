package com.benkolera.Rt

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
  import QueryBuilder.{Query,OrderBy}

  def query(query:Query,orderBy:Option[OrderBy])(
    implicit m:Monad[Future]
  ) = {
    queryRaw(
      QueryBuilder.buildQueryString( query ),
      orderBy.map( QueryBuilder.buildOrderByString( _ ) )
    )
  }

  def queryRaw(query:String,orderBy:Option[String])(
    implicit m:Monad[Future]
  ) = {
    val queryMap = ("query",query) ::
      orderBy.map( o => List("orderby" -> o) ).getOrElse(Nil)

    for {
      req  <- rtApi.map( _ / "search" / "ticket" << queryMap )
      body <- callApi( req )
      res  <- liftParseError(Parser.Query.parseQueryResponse(body))
    } yield res
  }

}
