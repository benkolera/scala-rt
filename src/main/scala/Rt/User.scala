package com.benkolera.Rt

import org.joda.time.DateTime

case class User (
  id: Int,
  name: String ,
  email: Option[String],
  realName: String,
  nickName: Option[String],
  comments: Option[String],
  mobilePhone: Option[String],
  privileged: Boolean,
  disabled: Boolean
)

object User {

  import dispatch._
  import scalaz._
  import syntax.traverse._
  import std.scalaFuture._
  import std.list._
  import syntax.std.boolean._
  import scala.concurrent.Future
  import scala.util.matching.Regex
  import QueryBuilder.{Query,OrderBy}

  def byId(id:Int)( implicit m:Monad[Future] ):RtM[Option[User]] = {
    for {
      req  <- rtApi.map( _ / "user" / id )
      body <- callApi( req )
      t    <- liftParseError(Parser.User.parseUser(body))
    } yield t
  }

}
