import sbt._
import Keys._

object Dependencies {

  lazy val additionalResolvers = Seq(
    Resolver.sonatypeRepo("snapshots"),
    "BaseX" at "http://files.basex.org/maven/",
    "xqj"   at "http://xqj.net/maven/")

  lazy val jettyVersion = "8.0.4.v20111024"

  lazy val prolog4jVersion = "0.2.1-SNAPSHOT"

  lazy val xmlrpcVersion = "3.1.2"

  lazy val coreDeps = Seq(
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

  lazy val depsSettings = Seq(
    resolvers ++= additionalResolvers,
    libraryDependencies ++= coreDeps ++ Seq(
      "org.scala-lang" % "scala-actors"  % scalaVersion.value,
      "org.scala-lang" % "scala-reflect" % scalaVersion.value))
}
