import BNFC._

autoCompilerPlugins in ThisBuild := true

addCompilerPlugin("org.scala-lang.plugins" % "continuations" % "2.10.5")

lazy val commonOptions = Seq(
  // ================================
  // TODO Eliminate feature warnings:
  // ================================
  // "-feature",
  // "-language:higherKinds",
  // "-language:implicitConversions",
  // "-language:postfixOps",
  // "-language:reflectiveCalls",
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
  // "-deprecation",
  // "-unchecked"
  // ============================
  // Original flags from pom.xml:
  // ============================
  "-encoding", "UTF-8",
  "-P:continuations:enable")

lazy val additionalResolvers = Seq(
  Resolver.bintrayRepo("synereo", "maven"),
  Resolver.sonatypeRepo("snapshots"),
  "BaseX" at "http://files.basex.org/maven/",
  "xqj"   at "http://xqj.net/maven/")

lazy val json4sVersion = "3.2.7"

lazy val prolog4jVersion = "0.2.1-SNAPSHOT"

lazy val specialKVersion = "1.1.8.5-28ba53a"

lazy val coreDeps = Seq(
  // =================================================
  // These dependencies appear to be currently unused:
  // =================================================
  // "org.apache.ws.commmons.util" % "ws-commons-util"   % "1.0.2",
  // "net.lag"                     % "configgy"          % "2.0.0",
  // ===============================================================
  // These dependencies are currently unmanaged (see lib directory):
  // ===============================================================
  // "org.prolog4j"                % "prolog4j-api"      % prolog4jVersion,
  // "org.prolog4j"                % "prolog4j-tuprolog" % prolog4jVersion,
  // =====================
  // Current dependencies:
  // =====================
  "biz.source_code"             % "base64coder"       % "2010-09-21",
  "com.biosimilarity.lift"     %% "specialk"          % specialKVersion,
  "com.rabbitmq"                % "amqp-client"       % "2.6.1",
  "com.thoughtworks.xstream"    % "xstream"           % "1.4.4" exclude("xmlpull", "xmlpull"),
  "com.typesafe"                % "config"            % "1.0.0",
  "commons-pool"                % "commons-pool"      % "1.6",
  "it.unibo.alice.tuprolog"     % "tuprolog"          % "2.1.1",
  "javax.persistence"           % "persistence-api"   % "1.0",
  "log4j"                       % "log4j"             % "1.2.17",
  "net.spy"                     % "spymemcached"      % "2.9.0",
  "org.basex"                   % "basex-api"         % "7.6"   exclude("org.slf4j", "slf4j-nop"),
  "org.codehaus.jettison"       % "jettison"          % "1.3.3",
  "org.json4s"                 %% "json4s-jackson"    % json4sVersion,
  "org.json4s"                 %% "json4s-native"     % json4sVersion,
  "org.mongodb"                %% "casbah"            % "2.6.4" exclude("org.slf4j", "slf4j-api"),
  "org.scala-lang"             %% "scala-pickling"    % "0.8.0",
  "org.scalesxml"              %% "scales-xml"        % "0.4.5",
  "junit"                       % "junit"             % "4.8.1"   % "test",
  "org.scalatest"              %% "scalatest"         % "2.0.M5b" % "test",
  "org.specs2"                 %% "specs2"            % "1.14"    % "test")

lazy val depsSettings = Seq(
  resolvers ++= additionalResolvers,
  libraryDependencies ++= coreDeps ++ Seq(
    "org.scala-lang" % "scala-actors"  % scalaVersion.value,
    "org.scala-lang" % "scala-reflect" % scalaVersion.value))

lazy val commonSettings = Seq(
  name := "agentservices-store-ia",
  organization := "com.protegra-ati",
  git.baseVersion := "1.9.5",
  git.formattedShaVersion := git.gitHeadCommit.value.map { sha =>
    s"${git.baseVersion.value}-${sha.substring(0, 7)}"
  },
  scalaVersion := "2.10.5",
  scalacOptions := commonOptions,
  fork in Test := true)

lazy val agentService = (project in file("."))
  .settings(commonSettings: _*)
  .settings(depsSettings: _*)
  .settings(bnfcSettings: _*)
  .enablePlugins(GitVersioning)
