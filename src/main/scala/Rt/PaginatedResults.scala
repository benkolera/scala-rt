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

  def paginateWSubQuery[A,B](
    qr: List[A] ,
    page:Int ,
    width: Int
  )(
    subQuery: NonEmptyList[A] => RtM[List[B]]
  )(
    implicit m:Monad[Future]
  ):RtM[PaginatedResults[B]] = {
    def doSubQuery( rawList:List[A] ) = rawList match {
      case Nil     => List[B]().point[RtM]
      case a::rest => subQuery( NonEmptyList( a , rest:_* ) )
    }

    for {
      pgres <- EitherT( calculatePagination(qr,page,width).point[RtRws] )
      bs    <- doSubQuery( pgres.results )
    } yield pgres.copy( results = bs )
  }

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
      pgres <- EitherT( calculatePagination(qr,page,width).point[RtRws] )
      bs    <- pgres.results.map( reifier ).sequenceU
    } yield pgres.copy( results = bs )
  }

  def calculatePagination[A](
    qr: List[A] ,
    page:Int ,
    width: Int
  ): InvalidPagination \/ PaginatedResults[A] = {
    for {
      cnt <- calculatePageCount( qr.length, width )
      _   <- validatePage( page, cnt )
    } yield PaginatedResults(
      qr.drop( (page-1) * width ).take( width ),
      page,
      width,
      cnt,
      qr.length
    )

  }

  def validatePage( page:Int , pageCount:Int ) = {
    if ( page < 1 || page > pageCount )
      -\/(InvalidPagination("Page should be between 1 and " + pageCount ))
    else
      \/-(())
  }

  def calculatePageCount( total:Int , pageWidth:Int ) = {
    if ( pageWidth <= 0 )
      -\/(InvalidPagination("Page width should be greater than 0"))
    else
      \/-(Math.max(1 , Math.ceil( total / pageWidth.toDouble ).toInt ) )
  }

}
