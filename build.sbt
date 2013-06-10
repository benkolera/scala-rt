organization := "com.example"

name := "figi"

version := "0.1.0-SNAPSHOT"

scalaVersion := "2.10.1"

libraryDependencies ++= Seq(
  "net.databinder" %% "unfiltered-netty-server" % "0.6.8",
  "net.databinder.dispatch" %% "dispatch-core" % "0.10.0",
  "org.clapper" %% "avsl" % "1.0.1",
  "net.databinder" %% "unfiltered-spec" % "0.6.8" % "test",
  "org.scalaz" %% "scalaz-core" % "7.0.0",
  "org.scalaz" %% "scalaz-effect" % "7.0.0",
  "org.typelevel" %% "scalaz-contrib-210" % "0.1.4" ,
  "com.github.scala-incubator.io" %% "scala-io-core" % "0.4.2"
)

resolvers ++= Seq(
  "jboss repo" at "http://repository.jboss.org/nexus/content/groups/public-jboss/"
)
