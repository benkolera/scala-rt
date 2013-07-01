package Rt

sealed trait Link
case class ExtUrl( url: String ) extends Link
case class RtTicket( id: Int ) extends Link
