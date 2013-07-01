import dispatch._ , Defaults._

import com.ning.http.client.{Request,AsyncHandler}

object Overloads {

  def getWebPage(): Future[Res] = {
    Http(url("http://www.google.com"))
  }

  //dispatch.Http.apply() is overloaded, so if I want to pass in that function
  //as a parameter so that I can unit test, then I either have to pick one:
  def getWebPage2( http: (Req => Future[Res]) ): Future[Res] = {
    http(url("http://www.google.com"))
  }

  //Or if I need more than one, write an ADT that handles the cases that I need
  sealed trait ReqAdt
  case class Builder( builder: Req ) extends ReqAdt
  case class Handler[A]( pair: (Request,AsyncHandler[A]) ) extends ReqAdt

  //So that I can then inject a function that handles all cases
  def getWebPage3( http: (ReqAdt => Future[Res]) ):Future[Res] = {
    http( Builder( url("http://google.com") ) )
    //OR
    http( Handler[String]( url("http://google.com") OK as.String ) )
  }

  //This isn't hard to implement and get around, but it is a bit of a bother
  //that would not be there if we used adts instead of overloading.

  //Or I'm just stupid and making a deal out of something trivial. ;)

}
