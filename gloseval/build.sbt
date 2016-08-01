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

lazy val agentServicesVersion = "1.9.5-ef3c169"

lazy val json4sVersion = "3.2.7"

lazy val prolog4jVersion = "0.2.1-SNAPSHOT"

lazy val specialKVersion = "1.1.8.5-28ba53a"

lazy val sprayVersion = "1.1.3"

lazy val coreDeps = Seq(
  // =================================================
  // These dependencies appear to be currently unused:
  // =================================================
  // "com.thoughtworks.xstream"    % "xstream"                % "1.4.4" exclude("xmlpull", "xmlpull"),
  // "commons-pool"                % "commons-pool"           % "1.6",
  // "log4j"                       % "log4j"                  % "1.2.17",
  // "org.apache.ws.commmons.util" % "ws-commons-util"        % "1.0.2",
  // "org.codehaus.jettison"       % "jettison"               % "1.3",
  // "org.scalaz"                 %% "scalaz-core"            % "7.0.0",
  // "org.slf4j"                   % "sl4j-log4j12"           % "1.7.18",
  // ===============================================================
  // These dependencies are currently unmanaged (see lib directory):
  // ===============================================================
  // "org.prolog4j"                % "prolog4j-api"           % prolog4jVersion,
  // "org.prolog4j"                % "prolog4j-tuprolog"      % prolog4jVersion,
  // "com.protegra-ati"           %% "agentservices-store-ia" % agentServicesVersion,
  // =====================
  // Current dependencies:
  // =====================
  "biz.source_code"             % "base64coder"            % "2010-09-21",
  "io.spray"                    % "spray-can"              % sprayVersion,
  "io.spray"                    % "spray-client"           % sprayVersion,
  "io.spray"                    % "spray-routing"          % sprayVersion,
  "io.spray"                    % "spray-testkit"          % sprayVersion,
  "io.spray"                   %% "spray-json"             % "1.2.5",
  "com.biosimilarity.lift"     %% "specialk"               % specialKVersion,
  "com.googlecode.json-simple"  % "json-simple"            % "1.1.1",
  "com.rabbitmq"                % "amqp-client"            % "2.6.1",
  "com.typesafe.akka"          %% "akka-actor"             % "2.1.4",
  "it.unibo.alice.tuprolog"     % "tuprolog"               % "2.1.1",
  "org.apache.commons"          % "commons-email"          % "1.3.1",
  "org.json4s"                 %% "json4s-jackson"         % json4sVersion,
  "org.json4s"                 %% "json4s-native"          % json4sVersion,
  "org.mongodb"                %% "casbah"                 % "2.6.4" exclude("org.slf4j", "slf4j-api"),
  "org.scalaj"                 %% "scalaj-http"            % "2.0.0",
  "org.specs2"                 %% "specs2"                 % "1.14" % "test")

lazy val depsSettings = Seq(
  resolvers ++= additionalResolvers,
  libraryDependencies ++= coreDeps ++ Seq(
    "org.scala-lang" % "scala-actors"  % scalaVersion.value,
    "org.scala-lang" % "scala-reflect" % scalaVersion.value,
    // https://github.com/json4s/json4s/issues/108
    "org.scala-lang" % "scalap"        % scalaVersion.value))

lazy val commonSettings = Seq(
  name := "GLoSEval",
  organization := "com.biosimilarity",
  git.baseVersion := "0.1",
  git.formattedShaVersion := git.gitHeadCommit.value.map { sha =>
    s"${git.baseVersion.value}-${sha.substring(0, 7)}"
  },
  scalaVersion := "2.10.5",
  scalacOptions := commonOptions,
  licenses += ("Apache-2.0", url("https://www.apache.org/licenses/LICENSE-2.0.html")),
  bintrayOrganization := Some("synereo"),
  test in assembly := {},
  fork in Test := true)

lazy val gloseval = (project in file("."))
  .settings(commonSettings: _*)
  .settings(depsSettings: _*)
  .enablePlugins(GitVersioning)
  .enablePlugins(JavaAppPackaging)
