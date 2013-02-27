organization  := "com.example"

version       := "0.1"

scalaVersion  := "2.10.0"

scalacOptions := Seq("-unchecked", "-deprecation", "-encoding", "utf8")

resolvers ++= Seq(
  "spray repo" at "http://repo.spray.io/",
  "json4s repo" at "http://repo.scala-sbt.org/scalasbt/repo/",
  "biosim repo" at "http://biosimrepomirror.googlecode.com/svn/trunk/"
)

libraryDependencies ++= Seq(
  "io.spray"                %   "spray-http"    % "1.1-M7",  
  "io.spray"                %   "spray-servlet" % "1.1-M7",
  "io.spray"                %   "spray-routing" % "1.1-M7",
  "io.spray"                %   "spray-testkit" % "1.1-M7",
  //"io.spray"                %   "spray-json_2.10" % "1.2.3",
  "org.json4s"              %   "json4s-native_2.10" % "3.1.0",
  "org.eclipse.jetty"       %   "jetty-webapp"  % "8.1.7.v20120910"     % "container",
  "org.eclipse.jetty.orbit" %   "javax.servlet" % "3.0.0.v201112011016" % "container"  artifacts Artifact("javax.servlet", "jar", "jar"),
  "com.typesafe.akka"       %%  "akka-actor"    % "2.1.0",
  "org.specs2"              %%  "specs2"        % "1.13" % "test",
  "com.biosimilarity.lift"  %  "specialK"      % "1.1.1.15"
)

seq(webSettings: _*)

port in container.Configuration := 9876
