package com.benkolera.Rt.Formatter

import com.benkolera.Rt
import org.joda.time.DateTimeZone

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
      ticket.customFields.toList.map(
        t => Rt.CustomFieldName.systemName(t._1) -> t._2.toString
      )
    ))
  }

}
