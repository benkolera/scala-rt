Warning - Crappy Alpha Software!
================================

This is a first go at a scala based interface to RT. It hasn't been production
tested and is still pretty crusty codebase-wise yet so use at your own peril! 

Installation
============
This isn't on maven central, so you'll have to install by going:

* git clone https://github.com/benkolera/scala-rt.git
* sbt publish-local

Use
===
Everything is a monad, so a call (e.g. Rt.Tickets.Query) doesn't actually do
anything to RT; it returns a program that can either be composed with other
rt RtMs (by flatMapping them together) or it can be run by giving the program
the configuration that it needs to do its job.

For example

```scala
import Rt.QueryBuilder._ , .Implicits._
val program = Rt.login.flatMap(
  _ => Rt.Tickets.query(
    Queue.eqs("dev") AND (Status.eqs("new") OR Status.eqs("open"))
  )
)

//OR (they are both the same)

val program = for {
  _       <- Rt.login
  tickets <- Rt.Tickets.Query( Queue.eqs("dev") AND (Status.eqs("new") OR Status.eqs("open")) )
} yield tickets

import concurrent.ExecutionContext.Implicits.global
val tickets r.run.run(Rt.Config("user","pass","rt.test.com"),Rt.emptyCookieJar)
```

