package Rt.Formatter

object Take {

  def toContentString( id: Int , action: String ):String = {
    fieldsToContentString(
      List(
        "Ticket" -> id.toString ,
        "Action" -> action
      )
    )
  }

}
