import BNFC._
import com.typesafe.sbt.packager.docker.{Cmd, ExecCmd}

lazy val ourScalaVersion    = "2.11.8"
lazy val amqpClientVersion  = "2.6.1"
lazy val base64coderVersion = "2010-09-21"
lazy val baseXVersion       = "7.6"
lazy val commonsPoolVersion = "1.6"
lazy val configVersion      = "1.3.1"
lazy val casbahVersion      = "3.1.1"
lazy val persistenceVersion = "1.0"
lazy val jettisonVersion    = "1.3.8"
lazy val jettyVersion       = "8.0.4.v20111024"
lazy val json4sVersion      = "3.4.1"
lazy val log4jVersion       = "1.2.17"
lazy val prolog4jVersion    = "0.2.1"
lazy val scalatestVersion   = "2.2.6"
lazy val scalesXmlVersion   = "0.6.0-M3"
lazy val slf4jVersion       = "1.7.21"
lazy val sprayVersion       = "1.3.3"
lazy val tuprologVersion    = "2.1.1"
lazy val xstreamVersion     = "1.4.9"

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

lazy val commonDeps = Seq(
  "log4j"     % "log4j"         % log4jVersion,
  "org.slf4j" % "slf4j-api"     % slf4jVersion,
  "org.slf4j" % "slf4j-log4j12" % slf4jVersion)

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
    Seq(compilerPlugin("org.scala-lang.plugins" % s"scala-continuations-plugin_$ourScalaVersion" % "1.0.2")) ++
    Seq("org.scala-lang"          % "scala-actors"                % scalaVersion.value,
        "org.scala-lang"          % "scala-reflect"               % scalaVersion.value,
        "org.scala-lang.plugins" %% "scala-continuations-library" % "1.0.2",
        // https://github.com/json4s/json4s/issues/108
        "org.scala-lang"          % "scalap"                      % scalaVersion.value) ++
    commonDeps)


lazy val specialkDeps = Seq(
  "biz.source_code"           % "base64coder"       % base64coderVersion,
  "com.rabbitmq"              % "amqp-client"       % amqpClientVersion,
  "com.thoughtworks.xstream"  % "xstream"           % xstreamVersion,
  "com.typesafe"              % "config"            % configVersion,
  "commons-pool"              % "commons-pool"      % commonsPoolVersion,
  "it.unibo.alice.tuprolog"   % "tuprolog"          % tuprologVersion,
  "javax.persistence"         % "persistence-api"   % persistenceVersion,
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
  "biz.source_code"             % "base64coder"              % base64coderVersion,
  "io.spray"                   %% "spray-can"                % sprayVersion,
  "io.spray"                   %% "spray-client"             % sprayVersion,
  "io.spray"                   %% "spray-httpx"              % sprayVersion,
  "io.spray"                   %% "spray-routing-shapeless2" % sprayVersion,
  "io.spray"                   %% "spray-testkit"            % sprayVersion,
  "io.spray"                   %% "spray-json"               % "1.3.2",
  "com.github.scopt"           %% "scopt"                    % "3.5.0",
  "com.rabbitmq"                % "amqp-client"              % amqpClientVersion,
  "com.typesafe.akka"          %% "akka-actor"               % "2.4.9",
  "it.unibo.alice.tuprolog"     % "tuprolog"                 % tuprologVersion,
  "org.apache.commons"          % "commons-email"            % "1.3.1",
  "org.json4s"                 %% "json4s-jackson"           % json4sVersion,
  "org.json4s"                 %% "json4s-native"            % json4sVersion,
  "org.mongodb"                %% "casbah"                   % casbahVersion exclude("org.slf4j", "slf4j-api"),
  "org.prolog4j"                % "prolog4j-api"             % prolog4jVersion,
  "org.prolog4j"                % "prolog4j-tuprolog"        % prolog4jVersion,
  "org.scalaj"                 %% "scalaj-http"              % "2.0.0",
  "org.bouncycastle"            % "bcprov-jdk15on"           % "1.54",
  "org.scalatest"              %% "scalatest"                % scalatestVersion % "test")

val buildBaseImage      = taskKey[Unit]("Builds the 'synereo/base' Docker Image")
val copyClientResources = taskKey[Unit]("Copy the 'client' directory to the Docker staging directory")

lazy val glosevalDockerSettings = Seq(
  mappings in Universal := {
    val deduped: Seq[(File, String)] =
      (mappings in Universal).value
        .map((x: (File, String)) => x.swap)
        .toMap
        .toSeq
        .map((x: (String, File)) => x.swap)
    deduped
      .:+(baseDirectory.value / "src" / "main" / "docker" / "node" / "eval.conf" -> "eval.conf")
      .:+(baseDirectory.value / "src" / "main" / "docker" / "node" / "log.conf" -> "log.conf")
      .:+(baseDirectory.value / "src" / "main" / "docker" / "node" / "supervisord.conf" -> "supervisord.conf")
  },
  buildBaseImage := {
    val cmd = s"docker build -t synereo/base:latest ${baseDirectory.value}/src/main/docker/base"
    Process(cmd) !
  },
  copyClientResources := {
    val sourceDir = baseDirectory.value / "client"
    val destDir = stagingDirectory.in(Docker).value / "opt" / "docker" / "client"
    IO.createDirectory(destDir)
    IO.copyDirectory(sourceDir, destDir)
  },
  stage in Docker := {
    buildBaseImage.value
    copyClientResources.value
    (stage in Docker).value
  },
  dockerCommands := {
    val localhostIpAddress = "127.0.0.1"
    val localhostRabbitPort = "5672"
    Seq(
      Cmd("FROM", "synereo/base"),
      Cmd("ENV", "DEPLOYMENT_MODE=colocated"),
      Cmd("ENV", s"DSL_COMM_LINK_CLIENT_HOSTS=$localhostIpAddress:$localhostRabbitPort"),
      Cmd("ENV", s"DSL_EVALUATOR_HOST=$localhostIpAddress"),
      Cmd("ENV", s"DSL_EVALUATOR_PORT=$localhostRabbitPort"),
      Cmd("ENV", s"DSL_EVALUATOR_PREFERRED_SUPPLIER_HOST=$localhostIpAddress"),
      Cmd("ENV", s"DSL_EVALUATOR_PREFERRED_SUPPLIER_PORT=$localhostRabbitPort"),
      Cmd("ENV", s"BFACTORY_COMM_LINK_SERVER_HOST=$localhostIpAddress"),
      Cmd("ENV", s"BFACTORY_COMM_LINK_SERVER_PORT=$localhostRabbitPort"),
      Cmd("ENV", s"BFACTORY_COMM_LINK_CLIENT_HOST=$localhostIpAddress"),
      Cmd("ENV", s"BFACTORY_COMM_LINK_CLIENT_PORT=$localhostRabbitPort"),
      Cmd("ENV", s"BFACTORY_EVALUATOR_HOST=$localhostIpAddress"),
      Cmd("ENV", s"BFACTORY_EVALUATOR_PORT=$localhostRabbitPort"),
      Cmd("WORKDIR", "/opt/docker"),
      Cmd("ADD", "opt", "/opt"),
      Cmd("USER", "root"),
      Cmd("RUN", "chown", "-R", "synereo:synereo", s"${defaultLinuxInstallLocation.in(Docker).value}"),
      Cmd("RUN", "chmod", "a+x", s"${defaultLinuxInstallLocation.in(Docker).value}/bin/${executableScriptName.value}"),
      Cmd("RUN", "mv", "supervisord.conf", "/etc/supervisor/conf.d/supervisord.conf"),
      Cmd("USER", "synereo"),
      Cmd("RUN", "bin/gloseval", "gencert", "--self-signed"),
      Cmd("USER", "root"),
      Cmd("EXPOSE", "8567", "9876"),
      ExecCmd("CMD", "/usr/bin/supervisord"))
  })

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
  .settings(glosevalDockerSettings: _*)
  .settings(commonSettings: _*)
  .dependsOn(specialk, agentService)
  .enablePlugins(JavaAppPackaging)
  .enablePlugins(DockerPlugin)
  .enablePlugins(BuildInfoPlugin)

lazy val worlockDeps = Seq(
  "com.github.docker-java"  % "docker-java"    % "3.0.6",
  "com.typesafe.akka"      %% "akka-actor"     % "2.4.9",
  "io.spray"               %% "spray-can"      % sprayVersion,
  "io.spray"               %% "spray-client"   % sprayVersion,
  "io.spray"               %% "spray-httpx"    % sprayVersion,
  "org.json4s"             %% "json4s-jackson" % json4sVersion,
  "org.json4s"             %% "json4s-native"  % json4sVersion,
  "org.scalatest"          %% "scalatest"      % scalatestVersion % "test")

val dockerTest = taskKey[Unit]("Run tests against Docker containers")

lazy val worlockSettings = Seq(
  name := "worlock",
  organization := "com.synereo",
  libraryDependencies ++= worlockDeps,
  dockerTest in Test := {
    publishLocal.in(Docker).in(gloseval).value
    test.in(Test).value
  })

lazy val worlock = (project in file("worlock"))
  .settings(worlockSettings: _*)
  .settings(commonSettings: _*)
  .dependsOn(gloseval % "test->test;compile->compile")

lazy val root = (project in file("."))
  .aggregate(specialk, agentService, gloseval, worlock)
  .dependsOn(specialk, agentService, gloseval, worlock)
  .settings(commonSettings: _*)
  .enablePlugins(GitVersioning)
