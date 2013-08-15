package com.benkolera.Rt

case class PaginatedResults[A](
  results: List[A],
  currentPage: Int,
  pageWidth: Int,
  pageCount:Int,
  resultCount: Int
)

object PaginatedResults {

  import dispatch._
  import scalaz._
  import syntax.monad._
  import syntax.traverse._
  import std.list._
  import scalaz.contrib.std.scalaFuture._
  import scala.concurrent.Future

  def paginate[A,B](
    qr: List[A] ,
    page:Int ,
    width: Int
  )(
    reifier: A => RtM[B]
  )(
    implicit m:Monad[Future]
  ):RtM[PaginatedResults[B]] = {
    for {
      cnt <- EitherT( calculatePageCount( qr.length, width ).point[RtRws] )
      _   <- EitherT( validatePage( page, cnt ).point[RtRws] )
      bs  <- qr.drop( (page-1) * width ).take( width ).map( reifier ).sequenceU
    } yield
      PaginatedResults( bs, page, width, cnt , qr.length )
  }

  def validatePage( page:Int , pageCount:Int )( implicit m:Monad[Future] ) = {
    if ( page < 1 || page > pageCount )
      -\/(InvalidPagination("Page should be between 1 and " + pageCount ))
    else
      \/-(())
  }

  def calculatePageCount( total:Int , pageWidth:Int )(
    implicit m:Monad[Future]
  ) = {
    if ( pageWidth <= 0 )
      -\/(InvalidPagination("Page width should be greater than 0"))
    else
      \/-(Math.max(1 , Math.ceil( total / pageWidth.toDouble ).toInt ) )
  }

}
