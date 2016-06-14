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
    // "org.prolog4j"              % "prolog4j-api"      % prolog4jVersion,
    // "org.prolog4j"              % "prolog4j-tuprolog" % prolog4jVersion,
    "it.unibo.alice.tuprolog"   % "tuprolog"          % "2.1.1",
    "com.google.zxing"          % "core"              % "2.0",
    "org.mongodb"              %% "casbah"            % "2.6.4",
    "org.json4s"               %% "json4s-jackson"    % "3.2.7",
    "biz.source_code"           % "base64coder"       % "2010-09-21",
    "org.coconut.forkjoin"      % "jsr166y"           % "070108",
    "com.h2database"            % "h2"                % "1.2.138",
    "junit"                     % "junit"             % "4.7"        % "test",
    "org.specs2"               %% "specs2"            % "1.14"       % "test",
    "org.scalatest"            %% "scalatest"         % "2.0.M5b"    % "test",
    "org.eclipse.jetty.orbit"   % "javax.servlet.jsp" % "2.2.0.v201112011158",
    "org.eclipse.jetty"         % "jetty-server"      % jettyVersion,
    "org.eclipse.jetty"         % "jetty-http"        % jettyVersion,
    "org.eclipse.jetty"         % "jetty-servlets"    % jettyVersion,
    "org.eclipse.jetty"         % "jetty-webapp"      % jettyVersion,
    "org.eclipse.jetty"         % "jetty-websocket"   % jettyVersion,
    "com.typesafe"              % "config"            % "1.0.0",
    // "net.lag"                   % "configgy"          % "???",
    "com.rabbitmq"              % "amqp-client"       % "2.6.1",
    "org.codehaus.jettison"     % "jettison"          % "1.3",
    "xpp3"                      % "xpp3_min"          % "1.1.4c",
    "com.thoughtworks.xstream"  % "xstream"           % "1.4.4",
    "javax.persistence"         % "persistence-api"   % "1.0",
    "commons-pool"              % "commons-pool"      % "1.6",
    "org.apache.xmlrpc"         % "xmlrpc-client"     % xmlrpcVersion,
    "org.apache.xmlrpc"         % "xmlrpc-common"     % xmlrpcVersion,
    "org.apache.xmlrpc"         % "xmlrpc-server"     % xmlrpcVersion,
    "javax.xml.xquery"          % "xqj-api"           % "1.0",
    "org.basex"                 % "basex-api"         % "7.6",
    "log4j"                     % "log4j"             % "1.2.17",
    "org.scalesxml"            %% "scales-xml"        % "0.4.5")

  lazy val depsSettings = Seq(
    resolvers ++= additionalResolvers,
    libraryDependencies ++= coreDeps ++ Seq(
      "org.scala-lang" % "scala-actors"  % scalaVersion.value,
      "org.scala-lang" % "scala-reflect" % scalaVersion.value))
}
