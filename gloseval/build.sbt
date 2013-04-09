organization  := "com.biosimilarity"

version       := "0.1"

scalaVersion  := "2.10.0"

autoCompilerPlugins := true

scalacOptions := Seq("-unchecked", "-deprecation", "-encoding", "utf8",
  "-feature", "-P:continuations:enable")

resolvers ++= Seq(
  "spray repo" at "http://repo.spray.io/",
  "json4s repo" at "http://repo.scala-sbt.org/scalasbt/repo/",
  "biosim repo" at "http://biosimrepomirror.googlecode.com/svn/trunk/",
  "scalaz repo" at "https://github.com/scalaz/scalaz.git"
)

libraryDependencies ++= Seq(
  "io.spray"               %   "spray-can"          % "1.1-M7",
  "io.spray"               %   "spray-routing"      % "1.1-M7",
  "io.spray"               %   "spray-testkit"      % "1.1-M7",
  "com.typesafe.akka"      %%  "akka-actor"         % "2.1.0",
  "org.specs2"             %%  "specs2"             % "1.13" % "test",
  "org.json4s"             %   "json4s-native_2.10" % "3.1.0",
  "org.scalaz"             %%  "scalaz-core"        % "6.0.4",
  compilerPlugin("org.scala-lang.plugins" % "continuations" % "2.10.0")
)    
    
seq(Revolver.settings: _*)

