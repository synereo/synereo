organization  := "com.biosimilarity"

name := "GLoSEval"

version       := "0.1"

scalaVersion  := "2.10.0"
//scalaVersion  := "2.9.2"

autoCompilerPlugins := true

scalacOptions := Seq("-unchecked", "-deprecation", "-encoding", "utf8",
  "-P:continuations:enable")

resolvers ++= Seq(
  "spray repo" at "http://repo.spray.io/",
  "json4s repo" at "http://repo.scala-sbt.org/scalasbt/repo/",
  "biosim repo" at "http://biosimrepomirror.googlecode.com/svn/trunk/",
  "scalaz repo" at "https://github.com/scalaz/scalaz.git",
  "basex repo" at "http://files.basex.org/maven/",
  "basex-xqj repo" at "http://xqj.net/maven/",
  "local-maven-cache repo" at "file://" + Path.userHome.absolutePath + "/.m2/repository/"
)

// Change for remote install
publishTo := Some(Resolver.file("file",  new File(Path.userHome.absolutePath+"/.m2/repository")))

libraryDependencies ++= Seq(
  "io.spray"               %   "spray-can"          % "1.1-M7",
  "io.spray"               %   "spray-routing"      % "1.1-M7",
  "io.spray"               %   "spray-testkit"      % "1.1-M7",
  "com.typesafe.akka"      %%  "akka-actor"         % "2.1.0",
//  "com.typesafe.akka"      %  "akka-actor"         % "2.0.5",
  "org.specs2"             %%  "specs2"             % "1.13" % "test",
//  "org.specs2"             %   "specs2_2.9.2"       % "1.12.4.1",
  "org.json4s"             %   "json4s-native_2.10" % "3.1.0",
  "org.json4s"             %   "json4s-jackson_2.10" % "3.1.0",
//  "org.json4s"             %   "json4s-native_2.9.2" % "3.1.0",
//  "org.json4s"             %   "json4s-jackson_2.9.2" % "3.1.0",
  "org.scalaz"             %%  "scalaz-core"        % "6.0.4",
  "org.scala-lang"         %   "scala-actors"       % "2.10.0",
  "com.biosimilarity.lift" %   "specialK"           % "1.1.4.2",
  "com.protegra-ati"       %   "agentservices-store" % "1.2.0.2-IA",
  "com.rabbitmq"           %   "amqp-client"        % "2.6.1",
  "org.prolog4j"           %   "prolog4j-api"       % "0.2.1-SNAPSHOT",
  "it.unibo.alice.tuprolog" %  "tuprolog"           % "2.1.1",
  "com.thoughtworks.xstream" % "xstream"            % "1.4.2",
  "org.mongodb"            %   "casbah_2.9.2"       % "2.5.1",
  "org.basex"              %   "basex-api"          % "7.5",
  "biz.source_code"        %   "base64coder"        % "2010-09-21",
  compilerPlugin("org.scala-lang.plugins" % "continuations" % "2.10.0")
  //compilerPlugin("org.scala-lang.plugins" % "continuations" % "2.9.2")
)    
    
seq(Revolver.settings: _*)

