import BNFC._

lazy val ourScalaVersion    = "2.10.6"
lazy val amqpClientVersion  = "2.6.1"
lazy val base64coderVersion = "2010-09-21"
lazy val baseXVersion       = "7.6"
lazy val commonsPoolVersion = "1.6"
lazy val configVersion      = "1.0.0"
lazy val casbahVersion      = "2.6.4"
lazy val persistenceVersion = "1.0"
lazy val jettisonVersion    = "1.3.3"
lazy val jettyVersion       = "8.0.4.v20111024"
lazy val json4sVersion      = "3.2.7"
lazy val log4jVersion       = "1.2.17"
lazy val prolog4jVersion    = "0.2.1"
lazy val scalatestVersion   = "2.2.6"
lazy val scalesXmlVersion   = "0.4.5"
lazy val sprayVersion       = "1.1.3"
lazy val tuprologVersion    = "2.1.1"
lazy val xstreamVersion     = "1.4.4"

lazy val commonOptions = Seq(
  // =========================================
  // TODO Enable as many of these as possible:
  // =========================================
  // "-Xfatal-warnings",
  // "-Xfuture",
  // "-Xlint",
  // "-Yno-adapted-args",
  // "-Ywarn-dead-code",
  // "-Ywarn-numeric-widen",
  // "-Ywarn-value-discard",
  "-deprecation",
  "-encoding", "UTF-8",
  "-feature",
  "-language:higherKinds",
  "-language:implicitConversions",
  "-language:postfixOps",
  "-language:reflectiveCalls",
  "-unchecked",
  "-Xmax-classfile-name", "244",
  "-P:continuations:enable")

lazy val commonSettings = Seq(
  scalaVersion := ourScalaVersion,
  scalacOptions := commonOptions,
  autoCompilerPlugins := true,
  git.baseVersion := "2.0",
  git.formattedShaVersion := git.gitHeadCommit.value.map { sha =>
    s"${git.baseVersion.value}-${sha.substring(0, 7)}"
  },
  resolvers ++= Seq(
    Resolver.bintrayRepo("synereo", "maven"),
    Resolver.sonatypeRepo("snapshots"),
    "BaseX" at "http://files.basex.org/maven/",
    "xqj"   at "http://xqj.net/maven/"),
  libraryDependencies ++=
    Seq(compilerPlugin("org.scala-lang.plugins" % "continuations" % scalaVersion.value)) ++
    Seq("org.scala-lang" % "scala-actors"  % scalaVersion.value,
        "org.scala-lang" % "scala-reflect" % scalaVersion.value,
        // https://github.com/json4s/json4s/issues/108
        "org.scala-lang" % "scalap"        % scalaVersion.value))

lazy val specialkDeps = Seq(
  "biz.source_code"           % "base64coder"       % base64coderVersion,
  "com.rabbitmq"              % "amqp-client"       % amqpClientVersion,
  "com.thoughtworks.xstream"  % "xstream"           % xstreamVersion,
  "com.typesafe"              % "config"            % configVersion,
  "commons-pool"              % "commons-pool"      % commonsPoolVersion,
  "it.unibo.alice.tuprolog"   % "tuprolog"          % tuprologVersion,
  "javax.persistence"         % "persistence-api"   % persistenceVersion,
  "log4j"                     % "log4j"             % log4jVersion,
  "org.basex"                 % "basex-api"         % baseXVersion exclude("org.slf4j", "slf4j-nop"),
  "org.coconut.forkjoin"      % "jsr166y"           % "070108",
  "org.codehaus.jettison"     % "jettison"          % jettisonVersion,
  "org.json4s"               %% "json4s-jackson"    % json4sVersion,
  "org.mongodb"              %% "casbah"            % casbahVersion exclude("org.slf4j", "slf4j-api"),
  "org.prolog4j"              % "prolog4j-api"      % prolog4jVersion,
  "org.prolog4j"              % "prolog4j-tuprolog" % prolog4jVersion,
  "org.scalacheck"           %% "scalacheck"        % "1.12.5" % "test",
  "org.scalatest"            %% "scalatest"         % scalatestVersion % "test",
  "org.scalesxml"            %% "scales-xml"        % scalesXmlVersion)

lazy val specialkSettings = Seq(
  name := "specialk",
  organization := "com.biosimilarity.lift",
  libraryDependencies ++= specialkDeps,
  fork in Test := true)

lazy val specialk = (project in file("specialk"))
  .settings(specialkSettings: _*)
  .settings(commonSettings: _*)
  .settings(bnfcSettings: _*)
  .enablePlugins(GitVersioning)

lazy val agentServiceDeps = Seq(
  "biz.source_code"             % "base64coder"       % base64coderVersion,
  "com.rabbitmq"                % "amqp-client"       % amqpClientVersion,
  "com.thoughtworks.xstream"    % "xstream"           % xstreamVersion,
  "com.typesafe"                % "config"            % configVersion,
  "commons-pool"                % "commons-pool"      % commonsPoolVersion,
  "it.unibo.alice.tuprolog"     % "tuprolog"          % tuprologVersion,
  "javax.persistence"           % "persistence-api"   % persistenceVersion,
  "log4j"                       % "log4j"             % log4jVersion,
  "net.spy"                     % "spymemcached"      % "2.9.0",
  "org.basex"                   % "basex-api"         % baseXVersion exclude("org.slf4j", "slf4j-nop"),
  "org.codehaus.jettison"       % "jettison"          % jettisonVersion,
  "org.json4s"                 %% "json4s-jackson"    % json4sVersion,
  "org.json4s"                 %% "json4s-native"     % json4sVersion,
  "org.mongodb"                %% "casbah"            % casbahVersion exclude("org.slf4j", "slf4j-api"),
  "org.prolog4j"                % "prolog4j-api"      % prolog4jVersion,
  "org.prolog4j"                % "prolog4j-tuprolog" % prolog4jVersion,
  "org.scalesxml"              %% "scales-xml"        % scalesXmlVersion,
  "org.scalatest"              %% "scalatest"         % scalatestVersion % "test")

lazy val agentServiceSettings = Seq(
  name := "agentservices-store-ia",
  organization := "com.protegra-ati",
  libraryDependencies ++= agentServiceDeps,
  fork in Test := true)

lazy val agentService = (project in file("agent-service/AgentServices-Store"))
  .settings(agentServiceSettings: _*)
  .settings(commonSettings: _*)
  .settings(bnfcSettings: _*)
  .dependsOn(specialk)

lazy val glosevalDeps = Seq(
  "biz.source_code"             % "base64coder"       % base64coderVersion,
  "io.spray"                    % "spray-can"         % sprayVersion,
  "io.spray"                    % "spray-client"      % sprayVersion,
  "io.spray"                    % "spray-routing"     % sprayVersion,
  "io.spray"                    % "spray-testkit"     % sprayVersion,
  "io.spray"                   %% "spray-json"        % "1.2.5",
  "com.github.scopt"           %% "scopt"             % "3.5.0",
  "com.rabbitmq"                % "amqp-client"       % amqpClientVersion,
  "com.typesafe.akka"          %% "akka-actor"        % "2.1.4",
  "it.unibo.alice.tuprolog"     % "tuprolog"          % tuprologVersion,
  "log4j"                       % "log4j"             % log4jVersion,
  "org.apache.commons"          % "commons-email"     % "1.3.1",
  "org.json4s"                 %% "json4s-jackson"    % json4sVersion,
  "org.json4s"                 %% "json4s-native"     % json4sVersion,
  "org.mongodb"                %% "casbah"            % casbahVersion exclude("org.slf4j", "slf4j-api"),
  "org.prolog4j"                % "prolog4j-api"      % prolog4jVersion,
  "org.prolog4j"                % "prolog4j-tuprolog" % prolog4jVersion,
  "org.scalaj"                 %% "scalaj-http"       % "2.0.0",
  "org.slf4j"                   % "slf4j-api"         % "1.7.21",
  "org.slf4j"                   % "slf4j-log4j12"     % "1.7.21",
  "org.bouncycastle"            % "bcprov-jdk15on"    % "1.54",
  "org.scalatest"              %% "scalatest"         % scalatestVersion % "test")

lazy val glosevalSettings = Seq(
  name := "GLoSEval",
  organization := "com.biosimilarity",
  libraryDependencies ++= glosevalDeps,
  buildInfoKeys := Seq[BuildInfoKey](name, version, scalaVersion),
  buildInfoPackage := "com.biosimilarity.evaluator",
  fork := true,
  parallelExecution in Test := false)

lazy val gloseval = (project in file("gloseval"))
  .settings(glosevalSettings: _*)
  .settings(commonSettings: _*)
  .dependsOn(specialk, agentService)
  .enablePlugins(JavaAppPackaging)
  .enablePlugins(BuildInfoPlugin)

lazy val root = (project in file("."))
  .aggregate(specialk, agentService, gloseval)
  .dependsOn(specialk, agentService, gloseval)
  .settings(commonSettings: _*)
  .enablePlugins(GitVersioning)
