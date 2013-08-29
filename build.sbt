organization := "com.benkolera"

name := "rt"

scalaVersion := "2.10.2"

scalacOptions ++= Seq("-feature","-deprecation")

libraryDependencies ++= Seq(
  "net.databinder.dispatch" %% "dispatch-core" % "0.11.0",
  "org.scalaz" %% "scalaz-core" % "7.0.2",
  "org.scalaz" %% "scalaz-effect" % "7.0.2",
  "org.typelevel" %% "scalaz-contrib-210" % "0.1.4" ,
  "com.github.scala-incubator.io" %% "scala-io-core" % "0.4.2",
  "com.github.nscala-time" %% "nscala-time" % "0.4.2",
  "com.github.tototoshi" %% "scala-csv" % "0.8.0",
  "org.mockito" % "mockito-core" % "1.9.5" % "test",
  "org.specs2" %% "specs2" % "2.0" % "test"
)

resolvers ++= Seq(
  "jboss repo" at "http://repository.jboss.org/nexus/content/groups/public-jboss/"
)

//Make this publish to oss.sonatype.com later
publishTo <<= version { (v: String) =>
  val nexus = "http://jenkins.build.iseek.com.au:8081/nexus/"
  if (v.trim.endsWith("SNAPSHOT"))
    Some("snapshots" at nexus + "content/repositories/snapshots/")
  else
    Some("releases"  at nexus + "content/repositories/releases/")
}

credentials += Credentials(Path.userHome / ".ivy2" / ".credentials")
