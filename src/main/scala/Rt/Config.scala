package com.benkolera.Rt

import dispatch._
import scala.concurrent.{ExecutionContext,Future}
import java.util.TimeZone
import org.joda.time.DateTimeZone
import org.joda.time.format.{DateTimeFormat,DateTimeFormatter}

case class Config (
  username: String,
  password: String,
  serverUrl: String,
  dateTimeZone: DateTimeZone,
  dateTimeFormatter: DateTimeFormatter,
  exContext: ExecutionContext,
  http:     (Req => Future[Either[Throwable,Res]])
)

object Config {
  def makeConfig(
    username: String,
    password: String,
    serverUrl: String,
    timeZoneName: String,
    dateTimeFormatPattern: String = "EEE MMM dd HH:mm:ss YYYY" //Rt Default
  )(
    implicit ex: ExecutionContext
  ) = {
    Config(
      username,
      password,
      serverUrl,
      DateTimeZone.forTimeZone( TimeZone.getTimeZone( timeZoneName ) ),
      DateTimeFormat.forPattern(dateTimeFormatPattern),
      ex,
      (req:Req) => Http.apply(req).either
    )
  }
}
