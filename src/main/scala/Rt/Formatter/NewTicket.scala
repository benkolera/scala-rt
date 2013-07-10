package com.benkolera.Rt.Formatter

import com.benkolera.Rt

object NewTicket {

  def toContentString( ticket: Rt.NewTicket ):String = {
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
      ticket.starts.map( dt => "Starts" -> fieldDateTimeToString(dt) ),
      ticket.started.map( dt => "Started" -> fieldDateTimeToString(dt) ),
      ticket.due.map( dt => "Due" -> fieldDateTimeToString(dt) ),
      ticket.resolved.map( dt => "Resolved" -> fieldDateTimeToString(dt) ),
      ticket.timeEstimated.map( "TimeEstimated" -> _.toString ),
      ticket.timeWorked.map( "TimeWorked" -> _.toString ),
      ticket.timeLeft.map( "TimeLeft" -> _.toString )
    ).collect{ case Some(t) => t } ++ (
      ticket.customFields.map(
        t => Rt.CustomFieldName.systemName(t._1) -> t._2.str
      )
    ))
  }

}
