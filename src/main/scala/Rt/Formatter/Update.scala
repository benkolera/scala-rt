package Rt.Formatter

object Update {

  def toContentString( ticket: Rt.Ticket ):String = {
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
      "Starts" -> fieldOptDateTimeToString(ticket.dates.starts),
      "Started" -> fieldOptDateTimeToString(ticket.dates.started),
      "Due" -> fieldOptDateTimeToString(ticket.dates.due),
      "Resolved" -> fieldOptDateTimeToString(ticket.dates.resolved),
      "Told" -> fieldOptDateTimeToString(ticket.dates.told),
      "TimeEstimated" -> ticket.effort.timeEstimated.toString,
      "TimeWorked" -> ticket.effort.timeWorked.toString,
      "TimeLeft" -> ticket.effort.timeLeft.toString
    ) ++ (
      ticket.customFields.toList.map(
        t => Rt.CustomFieldName.systemName(t._1) -> t._2.str
      )
    ))
  }

}
