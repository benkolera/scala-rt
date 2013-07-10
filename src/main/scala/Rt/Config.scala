package com.benkolera.Rt

import dispatch._
import scala.concurrent.{ExecutionContext,Future}
import org.joda.time.format.{DateTimeFormat,DateTimeFormatter}

case class Config (
  username: String,
  password: String,
  hostname: String,
  dateTimeFormatter: DateTimeFormatter,
  exContext: ExecutionContext,
  http:     (Req => Future[Either[Throwable,Res]])
)

object Config {
  def makeConfig(
    username: String,
    password: String,
    hostname: String,
    dateTimeFormatPattern: String = "EEE MMM dd HH:mm:ss YYYY" //Rt Default
  )(
    implicit ex: ExecutionContext
  ) = {
    Config(
      username,
      password,
      hostname,
      DateTimeFormat.forPattern(dateTimeFormatPattern),
      ex,
      (req:Req) => Http.apply(req).either
    )
  }
}
