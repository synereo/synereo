addSbtPlugin("com.typesafe.sbt" % "sbt-git" % "0.8.5")

addSbtPlugin("com.typesafe.sbt" % "sbt-native-packager" % "1.2.0-M5")

addSbtPlugin("com.eed3si9n" % "sbt-buildinfo" % "0.6.1")

// https://github.com/sbt/sbt-git/issues/69
libraryDependencies += "org.slf4j" % "slf4j-nop" % "1.7.21"
