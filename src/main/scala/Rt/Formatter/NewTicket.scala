package com.benkolera.Rt.Formatter

import com.benkolera.Rt
import org.joda.time.DateTimeZone

object NewTicket {

  def toContentString( ticket: Rt.NewTicket , tz:DateTimeZone ):String = {
    val dateTimeToString = fieldDateTimeToString(tz) _
    fieldsToContentString(List(
      Some("id" -> "ticket/new"),
      Some("Subject" -> ticket.subject),
      Some("Queue" -> ticket.queue),
      Some("Text" -> ticket.text),
      ticket.status.map( "Status" -> _ ),
      ticket.owner.map( "Owner" -> _ ),
      Some( "Requestor" -> fieldListToString(ticket.requestors) ),
      Some( "Cc" -> fieldListToString(ticket.ccs) ),
      Some( "AdminCc" -> fieldListToString(ticket.adminCcs) ),
      ticket.priority.map( "Priority" -> _.toString ),
      ticket.finalPriority.map( "FinalPriority" -> _.toString ),
      ticket.initialPriority.map( "InitialPriority" -> _.toString ),
      ticket.starts.map( dt => "Starts" -> dateTimeToString(dt) ),
      ticket.started.map( dt => "Started" -> dateTimeToString(dt) ),
      ticket.due.map( dt => "Due" -> dateTimeToString(dt) ),
      ticket.resolved.map( dt => "Resolved" -> dateTimeToString(dt) ),
      ticket.timeEstimated.map( "TimeEstimated" -> _.toString ),
      ticket.timeWorked.map( "TimeWorked" -> _.toString ),
      ticket.timeLeft.map( "TimeLeft" -> _.toString )
    ).collect{ case Some(t) => t } ++ (
      ticket.customFields.toList.flatMap(
        t => t._2.map( v => Rt.CustomFieldName.systemName(t._1) -> v.toString )
      )
    ))
  }

}
