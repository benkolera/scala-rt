package com.benkolera.Rt
package Parser

import TicketHistory._

import scalaz._
import std.option._
import std.list._
import syntax.applicative._
import syntax.traverse._
import com.github.nscala_time.time.Imports._
import org.joda.time.format.DateTimeFormatter
import org.joda.time.DateTimeZone
import org.joda.time.DateTimeZone.UTC


object History {

  val attachRe = """(.+?) \((\d+(?:\.\d+)?)([bkmg])\)""".r
  val historyDtf = DateTimeFormat.forPattern( "yyyy-MM-dd HH:mm:ss" )
  def parseHistory(
    tz:DateTimeZone, responseStr: String
  ):Parser[List[TicketHistory]] = {

    val extractDateTime = {
      def inner(m:Map[String,String])(fieldName:String) = {
        Field.extractFieldDateTime(UTC)(historyDtf)(m)(fieldName).map(
          _.withZone(tz)
        )
      }
      inner _
    }

    case class ConstructorArgs(
      id: Int,
      ticketId: Int,
      desc: String,
      creator: String,
      created: DateTime,
      fieldMap: Map[String,String]
    )
    type ConstructorOut = Parser[TicketHistory]

    def attachFieldsToAttachments(
      fs: List[Field]
    ):Parser[List[AttachmentInfo]] = {
      def convertToBytes( sizeStr: String , multiplier: Long = 1) = {
        (sizeStr.toDouble * multiplier).toLong
      }

      def splitAttachmentName( f: Field ) = {
        f.value match {
          case attachRe(n,s,"b") => (n,convertToBytes(s)).point[Parser]
          case attachRe(n,s,"k") => (n,convertToBytes(s,1024)).point[Parser]
          case attachRe(n,s,"m") => (n,convertToBytes(s,1024*1024)).point[Parser]
          case attachRe(n,s,"g") => (n,convertToBytes(s,1024*1024*1024)).point[Parser]
          case _ => parserFail(InvalidField(
            s"Attachment ${f.name}" ,
            s"'${f.value}' is not parsable as an attachment"
          ))
        }
      }

      fs.toList.map( field =>
        splitAttachmentName( field ).map( attachNameBytes =>
          AttachmentInfo(
            field.name.toInt,
            attachNameBytes._1,
            attachNameBytes._2
          )
        )
      ).sequenceU
    }

    def watcherTypeToEnum( s:String ) = {
      EitherT.fromTryCatch(
        WatcherType.withName(s).point[Scalaz.Id]
      ).leftMap( _ =>
        InvalidField(
          "History/AddWatcher.Field",
          s"$s is not a recognised watcher type"
        )
      )
    }

    def emailRecordConstructor( a: ConstructorArgs ):ConstructorOut = {
      for {
        content      <- Field.extractField(a.fieldMap)("Content")
        attachFields <- Field.extractFieldFields(a.fieldMap)("Attachments")
        attachments  <- attachFieldsToAttachments( attachFields )
      } yield EmailRecord(
        id = a.id,
        ticketId = a.ticketId,
        description = a.desc,
        creator = a.creator,
        created = a.created,
        content = content,
        attachments = attachments
      )
    }


    def createConstructor( a: ConstructorArgs ):ConstructorOut = {
      for {
        content      <- Field.extractField(a.fieldMap)("Content")
        attachFields <- Field.extractFieldFields(a.fieldMap)("Attachments")
        attachments  <- attachFieldsToAttachments( attachFields )
        timeTaken    <- Field.extractFieldInt(a.fieldMap)("TimeTaken")
      } yield Create(
        id = a.id,
        ticketId = a.ticketId,
        description = a.desc,
        creator = a.creator,
        created = a.created,
        content = content,
        attachments = attachments,
        timeTaken = timeTaken
      )
    }
    def customFieldConstructor( a: ConstructorArgs ):ConstructorOut = {
      for {
        oldValue <- Field.extractField(a.fieldMap)("OldValue")
        newValue <- Field.extractField(a.fieldMap)("NewValue")
        fieldId  <- Field.extractFieldInt(a.fieldMap)("Field")
      } yield CustomField(
        id = a.id,
        ticketId = a.ticketId,
        description = a.desc,
        creator = a.creator,
        created = a.created,
        oldValue = oldValue,
        newValue = newValue,
        fieldId = fieldId
      )
    }
    def statusConstructor( a: ConstructorArgs ):ConstructorOut = {
      for {
        oldStatus <- Field.extractField(a.fieldMap)("OldValue")
        newStatus <- Field.extractField(a.fieldMap)("NewValue")
      } yield Status(
        id = a.id,
        ticketId = a.ticketId,
        description = a.desc,
        creator = a.creator,
        created = a.created,
        oldStatus = oldStatus,
        newStatus = newStatus
      )
    }
    def commentConstructor( a: ConstructorArgs ):ConstructorOut = {
      for {
        content      <- Field.extractField(a.fieldMap)("Content")
        attachFields <- Field.extractFieldFields(a.fieldMap)("Attachments")
        attachments  <- attachFieldsToAttachments( attachFields )
        subject      <- Field.extractField(a.fieldMap)("Data")
        timeTaken    <- Field.extractFieldInt(a.fieldMap)("TimeTaken")
      } yield Comment(
        id = a.id,
        ticketId = a.ticketId,
        description = a.desc,
        creator = a.creator,
        created = a.created,
        content = content,
        attachments = attachments,
        subject = subject,
        timeTaken = timeTaken
      )
    }
    def commentEmailRecordConstructor( a: ConstructorArgs ):ConstructorOut = {
      for {
        content      <- Field.extractField(a.fieldMap)("Content")
        attachFields <- Field.extractFieldFields(a.fieldMap)("Attachments")
        attachments  <- attachFieldsToAttachments( attachFields )
      } yield CommentEmailRecord(
        id = a.id,
        ticketId = a.ticketId,
        description = a.desc,
        creator = a.creator,
        created = a.created,
        content = content,
        attachments = attachments
      )
    }

    def correspondConstructor( a: ConstructorArgs ):ConstructorOut = {
      for {
        content      <- Field.extractField(a.fieldMap)("Content")
        attachFields <- Field.extractFieldFields(a.fieldMap)("Attachments")
        attachments  <- attachFieldsToAttachments( attachFields )
        subject      <- Field.extractField(a.fieldMap)("Data")
        timeTaken    <- Field.extractFieldInt(a.fieldMap)("TimeTaken")
      } yield Correspond(
        id = a.id,
        ticketId = a.ticketId,
        description = a.desc,
        creator = a.creator,
        created = a.created,
        content = content,
        attachments = attachments,
        subject = subject,
        timeTaken = timeTaken
      )
    }
    def addWatcherConstructor( a: ConstructorArgs ):ConstructorOut = {
      for {
        watcherTypeStr <- Field.extractField(a.fieldMap)("Field")
        watcherType    <- watcherTypeToEnum(watcherTypeStr)
        watcher        <- Field.extractField(a.fieldMap)("NewValue")
      } yield AddWatcher(
        id = a.id,
        ticketId = a.ticketId,
        description = a.desc,
        creator = a.creator,
        created = a.created,
        watcherType = watcherType,
        watcher = watcher
      )
    }
    def deleteWatcherConstructor( a: ConstructorArgs ):ConstructorOut = {
      for {
        watcherTypeStr <- Field.extractField(a.fieldMap)("Field")
        watcherType    <- watcherTypeToEnum(watcherTypeStr)
        watcher        <- Field.extractField(a.fieldMap)("OldValue")
      } yield DeleteWatcher(
        id = a.id,
        ticketId = a.ticketId,
        description = a.desc,
        creator = a.creator,
        created = a.created,
        watcherType = watcherType,
        watcher = watcher
      )
    }
    def addLinkConstructor( a: ConstructorArgs ):ConstructorOut = {
      for {
        url      <- Field.extractField(a.fieldMap)("NewValue")
        linkName <- Field.extractField(a.fieldMap)("Field")
      } yield AddLink(
        id = a.id,
        ticketId = a.ticketId,
        description = a.desc,
        creator = a.creator,
        created = a.created,
        url = url,
        linkName = linkName
      )
    }
    def deleteLinkConstructor( a: ConstructorArgs ):ConstructorOut = {
      for {
        url      <- Field.extractField(a.fieldMap)("OldValue")
        linkName <- Field.extractField(a.fieldMap)("Field")
      } yield DeleteLink(
        id = a.id,
        ticketId = a.ticketId,
        description = a.desc,
        creator = a.creator,
        created = a.created,
        url = url,
        linkName = linkName
      )
    }
    def addReminderConstructor( a: ConstructorArgs ):ConstructorOut = {
      for {
        reminderId <- Field.extractFieldInt(a.fieldMap)("NewValue")
      } yield AddReminder(
        id = a.id,
        ticketId = a.ticketId,
        description = a.desc,
        creator = a.creator,
        created = a.created,
        reminderId = reminderId
      )
    }
    def resolveReminderConstructor( a: ConstructorArgs ):ConstructorOut = {
      for {
        reminderId <- Field.extractFieldInt(a.fieldMap)("NewValue")
      } yield ResolveReminder(
        id = a.id,
        ticketId = a.ticketId,
        description = a.desc,
        creator = a.creator,
        created = a.created,
        reminderId = reminderId
      )
    }
    def setConstructor( a: ConstructorArgs ):ConstructorOut = {
      for {
        oldValue  <- Field.extractField(a.fieldMap)("OldValue")
        newValue  <- Field.extractField(a.fieldMap)("NewValue")
        fieldName <- Field.extractField(a.fieldMap)("Field")
      } yield Set(
        id = a.id,
        ticketId = a.ticketId,
        description = a.desc,
        creator = a.creator,
        created = a.created,
        fieldName = fieldName,
        oldValue = oldValue,
        newValue = newValue
      )
    }

    def toldConstructor( a: ConstructorArgs ):ConstructorOut = {
      for {
        date <- extractDateTime(a.fieldMap)("NewValue")
      } yield Told(
        id = a.id,
        ticketId = a.ticketId,
        description = a.desc,
        creator = a.creator,
        created = a.created,
        date = date
      )
    }

    val constructorMap = Map(
      "EmailRecord" -> emailRecordConstructor _ ,
      "Create" -> createConstructor _ ,
      "CustomField" -> customFieldConstructor _ ,
      "Status" -> statusConstructor _ ,
      "CommentEmailRecord" -> commentEmailRecordConstructor _ ,
      "Correspond" -> correspondConstructor _ ,
      "Comment" -> commentConstructor _ ,
      "AddWatcher" -> addWatcherConstructor _ ,
      "DelWatcher" -> deleteWatcherConstructor _ ,
      "AddLink" -> addLinkConstructor _ ,
      "DeleteLink" -> deleteLinkConstructor _ ,
      "AddReminder" -> addReminderConstructor _ ,
      "ResolveReminder" -> resolveReminderConstructor _ ,
      "Set" -> setConstructor _ ,
      "Told" -> toldConstructor _
    )

    def getConstructor( ticketType: String ) = {
      constructorMap.get(
        ticketType
      ).fold[ Parser[ConstructorArgs=>ConstructorOut] ](
        parserFail(UnknownHistoryType(ticketType))
      )( _.point[Parser] )
    }

    parseResponse( responseStr.split("\n").toList ).flatMap( lines =>
      splitMultipart( lines ).map( historyLines =>
        Field.parseFieldMap( historyLines ).flatMap( fieldMap => {
          val extInt      = Field.extractFieldInt( fieldMap )
          val extString   = Field.extractString( fieldMap )
          val extDateTime = extractDateTime( fieldMap )

          for {
            id          <- extInt( "id" )
            ticket      <- extInt("Ticket")
            desc        <- extString("Description")
            creator     <- extString("Creator")
            created     <- extDateTime("Created")
            constructor <- extString("Type").flatMap( getConstructor _ )
            history     <- constructor(
              ConstructorArgs(id,ticket,desc,creator,created,fieldMap)
            )
          } yield history
        })
      ).sequenceU
    )
  }

}
