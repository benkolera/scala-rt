organization := "com.benkolera"

name := "rt"

version := "0.1.0-SNAPSHOT"

scalaVersion := "2.10.1"

scalacOptions ++= Seq("-feature","-deprecation")

libraryDependencies ++= Seq(
  "net.databinder.dispatch" %% "dispatch-core" % "0.10.0",
  "org.clapper" %% "avsl" % "1.0.1",
  "org.scalaz" %% "scalaz-core" % "7.0.0",
  "org.scalaz" %% "scalaz-effect" % "7.0.0",
  "org.typelevel" %% "scalaz-contrib-210" % "0.1.4" ,
  "com.github.scala-incubator.io" %% "scala-io-core" % "0.4.2",
  "com.github.nscala-time" %% "nscala-time" % "0.4.2",
  "org.mockito" % "mockito-core" % "1.9.5" % "test",
  "org.specs2" %% "specs2" % "2.0" % "test"
)

resolvers ++= Seq(
  "jboss repo" at "http://repository.jboss.org/nexus/content/groups/public-jboss/"
)
