package Rt.Formatter

object Link {
  def updateToParamMap(
    ticketId:Int , rel:String, del: Boolean , link: Rt.Link
  ):Map[String,String] = {
    def expandLink( l:Rt.Link ) = l match {
      case Rt.RtTicketLink(id) => id.toString
      case Rt.ExtUrlLink(url)  => url
    }
    Map(
      "id"  -> ticketId.toString,
      "rel" -> rel,
      "del" -> (if ( del ) "1" else "0" ),
      "to"  -> expandLink(link)
    )
  }

}
