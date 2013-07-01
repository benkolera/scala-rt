package Rt.Formatter

import com.ning.http.client.{Part,StringPart,ByteArrayPart}

object TicketMessage {

  def toParts( action: String , id: Int, msg: Rt.TicketMessage ):List[Part] = {
    val contentStr = fieldsToContentString(
      List(
        Some("Action" -> action ),
        msg.subject.map( "Subject" -> _ ),
        Some("Cc" -> fieldListToString(msg.ccs) ),
        Some("Bcc" -> fieldListToString(msg.bccs) ),
        Some("TimeWorked" -> msg.timeWorked.toString ) ,
        Some("Text" -> msg.text) ,
        msg.attachments.headOption.map( _ => "Attachment" -> fieldListToString( msg.attachments.map( _.fileName ) ) )
      ).collect{ case Some(t) => t }
    )
    new StringPart("content",contentStr) ::
    msg.attachments.zipWithIndex.map{
      case (a,i) => new ByteArrayPart(
        s"attachment_${i+1}",a.fileName,a.data,a.mimeType,a.charSet
      )
    }
  }

}
