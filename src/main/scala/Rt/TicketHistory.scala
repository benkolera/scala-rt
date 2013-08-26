package com.benkolera.Rt

import org.joda.time.{DateTime,DateTimeZone}

case class AttachmentInfo(
  id: Int,
  name: String,
  size: Long
)

object WatcherType extends Enumeration {
  val Requestor,Cc,AdminCc = Value
}

sealed trait TicketHistory {
  def id: Int
  def ticketId: Int
  def description: String
  def creator: String
  def created: DateTime
}

object TicketHistory {

  case class EmailRecord(
    id: Int,
    ticketId:Int,
    description: String,
    creator: String,
    created: DateTime,
    content: String,
    attachments: List[AttachmentInfo]
  ) extends TicketHistory
  case class Create(
    id: Int,
    ticketId:Int,
    description: String,
    creator: String,
    created: DateTime,
    timeTaken: Int,
    content: String,
    attachments: List[AttachmentInfo]
  ) extends TicketHistory
  case class CustomField(
    id: Int,
    ticketId:Int,
    description: String,
    creator: String,
    created: DateTime,
    fieldId: Int,
    oldValue: String,
    newValue: String
  ) extends TicketHistory
  case class Status(
    id: Int,
    ticketId:Int,
    description: String,
    creator: String,
    created: DateTime,
    oldStatus: String,
    newStatus: String
  ) extends TicketHistory
  case class CommentEmailRecord (
    id: Int,
    ticketId:Int,
    description: String,
    creator: String,
    created: DateTime,
    content: String,
    attachments: List[AttachmentInfo]
  ) extends TicketHistory
  case class Correspond(
    id: Int,
    ticketId:Int,
    description: String,
    creator: String,
    created: DateTime,
    subject: String,
    content: String,
    timeTaken: Int,
    attachments: List[AttachmentInfo]
  ) extends TicketHistory
  case class Comment(
    id: Int,
    ticketId:Int,
    description: String,
    creator: String,
    created: DateTime,
    subject: String,
    content: String,
    timeTaken: Int,
    attachments: List[AttachmentInfo]
  ) extends TicketHistory
  case class AddWatcher(
    id: Int,
    ticketId:Int,
    description: String,
    creator: String,
    created: DateTime,
    watcherType: WatcherType.Value,
    watcher: String
  ) extends TicketHistory
  case class DeleteWatcher(
    id: Int,
    ticketId:Int,
    description: String,
    creator: String,
    created: DateTime,
    watcherType: WatcherType.Value,
    watcher: String
  ) extends TicketHistory
  case class AddLink(
    id: Int,
    ticketId:Int,
    description: String,
    creator: String,
    created: DateTime,
    linkName: String,
    url: String
  ) extends TicketHistory
  case class DeleteLink(
    id: Int,
    ticketId:Int,
    description: String,
    creator: String,
    created: DateTime,
    linkName: String,
    url: String
  ) extends TicketHistory
  case class AddReminder(
    id: Int,
    ticketId:Int,
    description: String,
    creator: String,
    created: DateTime,
    reminderId: Int
  ) extends TicketHistory
  case class ResolveReminder(
    id: Int,
    ticketId:Int,
    description: String,
    creator: String,
    created: DateTime,
    reminderId: Int
  ) extends TicketHistory
  case class Set(
    id: Int,
    ticketId:Int,
    description: String,
    creator: String,
    created: DateTime,
    fieldName: String,
    oldValue: String,
    newValue: String
  ) extends TicketHistory
  case class Told(
    id: Int,
    ticketId:Int,
    description: String,
    creator: String,
    created: DateTime,
    date: DateTime
  ) extends TicketHistory
  case class SystemError(
    id: Int,
    ticketId:Int,
    description: String,
    creator: String,
    created: DateTime
  ) extends TicketHistory
}
