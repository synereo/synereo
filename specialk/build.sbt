import BNFC._
import Dependencies.depsSettings

/*
 * NOTE: for dependencies and resolvers, see:
 *   ./project/Dependencies.scala
 */

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
  "-explaintypes",
  "-P:continuations:enable")

lazy val commonSettings = Seq(
  name := "specialK",
  organization := "com.biosimilarity.lift",
  version := "1.1.8.5",
  scalaVersion := "2.10.5",
  scalacOptions := commonOptions,
  fork in Test := true)

lazy val root = (project in file("."))
  .settings(commonSettings: _*)
  .settings(depsSettings: _*)
  .settings(bnfcSettings: _*)
