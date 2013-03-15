organization  := "com.biosimilarity"

version       := "0.1"

scalaVersion  := "2.10.0"

scalacOptions := Seq("-unchecked", "-deprecation", "-encoding", "utf8")

resolvers ++= Seq(
  "spray repo" at "http://repo.spray.io/",
  "json4s repo" at "http://repo.scala-sbt.org/scalasbt/repo/"
)

libraryDependencies ++= Seq(
  "io.spray"            %   "spray-can"     % "1.1-M7",
  "io.spray"            %   "spray-routing" % "1.1-M7",
  "io.spray"            %   "spray-testkit" % "1.1-M7",
  "com.typesafe.akka"   %%  "akka-actor"    % "2.1.0",
  "org.specs2"          %%  "specs2"        % "1.13" % "test",
  "org.json4s"              %   "json4s-native_2.10" % "3.1.0"
)

seq(Revolver.settings: _*)

