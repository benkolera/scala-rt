package Rt

import dispatch._
import scala.concurrent.{ExecutionContext,Future}

case class Config (
  username: String,
  password: String,
  hostname: String,
  exContext: ExecutionContext,
  http:     (Req => Future[Either[Throwable,Res]])
)

object Config {
  def makeConfig(username: String, password: String, hostname: String )(
    implicit ex: ExecutionContext
  ) = {
    Config(
      username,
      password,
      hostname,
      ex,
      (req:Req) => Http.apply(req).either
    )
  }
}
