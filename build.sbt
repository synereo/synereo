import BNFC._

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
  Resolver.sonatypeRepo("snapshots"),
  "BaseX" at "http://files.basex.org/maven/",
  "xqj"   at "http://xqj.net/maven/")

lazy val json4sVersion = "3.2.7"

lazy val jettyVersion = "8.0.4.v20111024"

lazy val prolog4jVersion = "0.2.1-SNAPSHOT"

lazy val xmlrpcVersion = "3.1.2"

lazy val specialkDeps = Seq(
  // =================================================
  // These dependencies appear to be currently unused:
  // =================================================
  // "com.google.zxing"          % "core"              % "2.0",
  // "com.h2database"            % "h2"                % "1.2.138",
  // "javax.xml.xquery"          % "xqj-api"           % "1.0",
  // "junit"                     % "junit"             % "4.7"     % "test",
  // "net.lag"                   % "configgy"          % "???",
  // "org.apache.xmlrpc"         % "xmlrpc-client"     % xmlrpcVersion exclude("javax.servlet", "servlet-api"),
  // "org.apache.xmlrpc"         % "xmlrpc-common"     % xmlrpcVersion exclude("javax.servlet", "servlet-api"),
  // "org.apache.xmlrpc"         % "xmlrpc-server"     % xmlrpcVersion exclude("javax.servlet", "servlet-api"),
  // "org.eclipse.jetty"         % "jetty-http"        % jettyVersion,
  // "org.eclipse.jetty"         % "jetty-server"      % jettyVersion exclude("org.mortbay.jetty", "servlet-api"),
  // "org.eclipse.jetty"         % "jetty-servlets"    % jettyVersion,
  // "org.eclipse.jetty"         % "jetty-webapp"      % jettyVersion,
  // "org.eclipse.jetty"         % "jetty-websocket"   % jettyVersion,
  // "org.eclipse.jetty.orbit"   % "javax.servlet.jsp" % "2.2.0.v201112011158",
  // "org.scalatest"            %% "scalatest"         % "2.0.M5b" % "test",
  // "org.specs2"               %% "specs2"            % "1.14"    % "test",
  // "xpp3"                      % "xpp3_min"          % "1.1.4c",
  // ===============================================================
  // These dependencies are currently unmanaged (see lib directory):
  // ===============================================================
  // "org.prolog4j"              % "prolog4j-api"      % prolog4jVersion,
  // "org.prolog4j"              % "prolog4j-tuprolog" % prolog4jVersion,
  // =====================
  // Current dependencies:
  // =====================
  "biz.source_code"           % "base64coder"       % "2010-09-21",
  "com.rabbitmq"              % "amqp-client"       % "2.6.1",
  "com.thoughtworks.xstream"  % "xstream"           % "1.4.4" exclude("xmlpull", "xmlpull"),
  "com.typesafe"              % "config"            % "1.0.0",
  "commons-pool"              % "commons-pool"      % "1.6",
  "it.unibo.alice.tuprolog"   % "tuprolog"          % "2.1.1",
  "javax.persistence"         % "persistence-api"   % "1.0",
  "log4j"                     % "log4j"             % "1.2.17",
  "org.basex"                 % "basex-api"         % "7.6",
  "org.coconut.forkjoin"      % "jsr166y"           % "070108",
  "org.codehaus.jettison"     % "jettison"          % "1.3",
  "org.json4s"               %% "json4s-jackson"    % "3.2.7",
  "org.mongodb"              %% "casbah"            % "2.6.4",
  "org.scalacheck"           %% "scalacheck"        % "1.12.5" % "test",
  "org.scalatest"            %% "scalatest"         % "2.2.6"  % "test",
  "org.scalesxml"            %% "scales-xml"        % "0.4.5")

lazy val specialkDepsSettings = Seq(
  resolvers ++= additionalResolvers,
  libraryDependencies ++= specialkDeps ++ Seq(
    "org.scala-lang" % "scala-actors"  % scalaVersion.value,
    "org.scala-lang" % "scala-reflect" % scalaVersion.value,
    // https://github.com/json4s/json4s/issues/108
    "org.scala-lang" % "scalap"        % scalaVersion.value))

lazy val specialkSettings = Seq(
  autoCompilerPlugins := true,
  addCompilerPlugin("org.scala-lang.plugins" % "continuations" % "2.10.5"),
  name := "specialk",
  organization := "com.biosimilarity.lift",
  git.baseVersion := "1.1.8.5",
  git.formattedShaVersion := git.gitHeadCommit.value.map { sha =>
    s"${git.baseVersion.value}-${sha.substring(0, 7)}"
  },
  scalaVersion := "2.10.5",
  scalacOptions := commonOptions,
  licenses += ("Apache-2.0", url("https://www.apache.org/licenses/LICENSE-2.0.html")),
  bintrayOrganization := Some("synereo"),
  fork in Test := true)

lazy val specialk = (project in file("specialk"))
  .settings(specialkSettings: _*)
  .settings(specialkDepsSettings: _*)
  .settings(bnfcSettings: _*)
  .enablePlugins(GitVersioning)

lazy val agentServiceDeps = Seq(
  // =================================================
  // These dependencies appear to be currently unused:
  // =================================================
  // "org.apache.ws.commmons.util" % "ws-commons-util"   % "1.0.2",
  // "net.lag"                     % "configgy"          % "2.0.0",
  // "org.scala-lang"             %% "scala-pickling"    % "0.8.0",
  // ===============================================================
  // These dependencies are currently unmanaged (see lib directory):
  // ===============================================================
  // "org.prolog4j"                % "prolog4j-api"      % prolog4jVersion,
  // "org.prolog4j"                % "prolog4j-tuprolog" % prolog4jVersion,
  // =====================
  // Current dependencies:
  // =====================
  "biz.source_code"             % "base64coder"       % "2010-09-21",
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
  "org.scalesxml"              %% "scales-xml"        % "0.4.5",
  "org.scalatest"              %% "scalatest"         % "2.2.6" % "test")

lazy val agentServiceDepsSettings = Seq(
  resolvers ++= additionalResolvers,
  libraryDependencies ++= agentServiceDeps ++ Seq(
    "org.scala-lang" % "scala-actors"  % scalaVersion.value,
    "org.scala-lang" % "scala-reflect" % scalaVersion.value))

lazy val agentServiceSettings = Seq(
  autoCompilerPlugins := true,
  addCompilerPlugin("org.scala-lang.plugins" % "continuations" % "2.10.5"),
  name := "agentservices-store-ia",
  organization := "com.protegra-ati",
  git.baseVersion := "1.9.5",
  git.formattedShaVersion := git.gitHeadCommit.value.map { sha =>
    s"${git.baseVersion.value}-${sha.substring(0, 7)}"
  },
  scalaVersion := "2.10.5",
  scalacOptions := commonOptions,
  fork in Test := true)

lazy val agentService = (project in file("agent-service/AgentServices-Store"))
  .settings(agentServiceSettings: _*)
  .settings(agentServiceDepsSettings: _*)
  .settings(bnfcSettings: _*)
  .dependsOn(specialk)
  .enablePlugins(GitVersioning)

lazy val sprayVersion = "1.1.3"

lazy val glosevalDeps = Seq(
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
  "com.googlecode.json-simple"  % "json-simple"            % "1.1.1",
  "com.rabbitmq"                % "amqp-client"            % "2.6.1",
  "com.typesafe.akka"          %% "akka-actor"             % "2.1.4",
  "it.unibo.alice.tuprolog"     % "tuprolog"               % "2.1.1",
  "org.apache.commons"          % "commons-email"          % "1.3.1",
  "org.json4s"                 %% "json4s-jackson"         % json4sVersion,
  "org.json4s"                 %% "json4s-native"          % json4sVersion,
  "org.mongodb"                %% "casbah"                 % "2.6.4" exclude("org.slf4j", "slf4j-api"),
  "org.scalaj"                 %% "scalaj-http"            % "2.0.0",
  "org.bouncycastle"            % "bcprov-jdk15on"         % "1.54",
  "org.scalatest"              %% "scalatest"              % "2.2.6" % "test")

lazy val glosevalDepsSettings = Seq(
  resolvers ++= additionalResolvers,
  libraryDependencies ++= glosevalDeps ++ Seq(
    "org.scala-lang" % "scala-actors"  % scalaVersion.value,
    "org.scala-lang" % "scala-reflect" % scalaVersion.value,
    // https://github.com/json4s/json4s/issues/108
    "org.scala-lang" % "scalap"        % scalaVersion.value))

lazy val glosevalSettings = Seq(
  autoCompilerPlugins := true,
  addCompilerPlugin("org.scala-lang.plugins" % "continuations" % "2.10.5"),
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
  fork in Test := true,
  parallelExecution in Test := false)

lazy val gloseval = (project in file("gloseval"))
  .settings(glosevalSettings: _*)
  .settings(glosevalDepsSettings: _*)
  .dependsOn(specialk, agentService)
  .enablePlugins(GitVersioning)
  .enablePlugins(JavaAppPackaging)

lazy val root = (project in file("."))
  .aggregate(specialk, agentService, gloseval)
  .dependsOn(specialk, agentService, gloseval)
  .settings(autoCompilerPlugins := true,
            addCompilerPlugin("org.scala-lang.plugins" % "continuations" % "2.10.5"),
            scalacOptions := commonOptions)
