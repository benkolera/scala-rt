package com.benkolera.Rt.Formatter

import com.benkolera.Rt
import org.joda.time.DateTimeZone
import com.github.tototoshi.csv._
import java.io.StringWriter

object Update {

  def toContentString( ticket: Rt.Ticket , tz:DateTimeZone ):String = {
    val optDateTimeToString = fieldOptDateTimeToString( tz ) _
    fieldsToContentString(List(
      "id" -> s"ticket/${ticket.id}",
      "Subject" -> ticket.subject,
      "Queue" -> ticket.queue,
      "Status" -> ticket.status,
      "Owner" -> ticket.people.owner.getOrElse("Nobody") ,
      "Requestors" -> fieldListToString(ticket.people.requestors) ,
      "Cc" -> fieldListToString(ticket.people.ccs),
      "AdminCc" -> fieldListToString(ticket.people.adminCcs),
      "Priority" -> ticket.priority.priority.toString ,
      "FinalPriority" -> ticket.priority.finalPriority.toString ,
      "InitialPriority" -> ticket.priority.initialPriority.toString ,
      "Starts" -> optDateTimeToString(ticket.dates.starts),
      "Started" -> optDateTimeToString(ticket.dates.started),
      "Due" -> optDateTimeToString(ticket.dates.due),
      "Resolved" -> optDateTimeToString(ticket.dates.resolved),
      "Told" -> optDateTimeToString(ticket.dates.told),
      "TimeEstimated" -> ticket.effort.timeEstimated.toString,
      "TimeWorked" -> ticket.effort.timeWorked.toString,
      "TimeLeft" -> ticket.effort.timeLeft.toString
    ) ++ (
      ticket.customFields.toList.map{ t =>
        val stringWriter = new StringWriter()
        val csvWriter    = CSVWriter.open(stringWriter)
        csvWriter.writeRow( t._2.map(_.toString) )
        csvWriter.flush()
        csvWriter.close()

        Rt.CustomFieldName.systemName(t._1) -> stringWriter.toString
      }
    ))
  }

}
