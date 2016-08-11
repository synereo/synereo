# GLoSEval

[![Build Status](https://travis-ci.org/synereo/gloseval.svg?branch=1.0)](https://travis-ci.org/synereo/gloseval)
[![Gitter](https://badges.gitter.im/synereo/gloseval.svg)](https://gitter.im/synereo/gloseval?utm_source=badge&utm_medium=badge&utm_campaign=pr-badge&utm_content=badge)

Network evaluator for Generalized Language of Streams.

Backend for the Synereo Social Platform.

## Installation

To work with GLoSEval you will need:

* [MongoDB](https://www.mongodb.com/), version 2.6.12 (also tested with version 2.4.14)
  * available at https://www.mongodb.com/download-center (go to "Previous Releases")
* [Erlang](https://www.erlang.org/), version 15B03 (also tested with version R14B04) (required to run RabbitMQ)
  * available at https://www.erlang-solutions.com/resources/download.html
* [RabbitMQ](http://www.rabbitmq.com/), version 3.0.2 (also tested with version 2.7.1)
  * available at http://www.rabbitmq.com/download.html (go to "Older Versions")
* Java Development Kit (JDK), version 7
  * available at http://www.oracle.com/technetwork/java/javase/downloads/jdk7-downloads-1880260.html
  * alternatively, the [OpenJDK](http://openjdk.java.net/) can be installed using most common package managers.
* [sbt](http://www.scala-sbt.org/)
  * available at http://www.scala-sbt.org/

After installing these dependencies, you can clone the GLoSEval repo and run it:
```
$ git clone https://github.com/synereo/gloseval.git
  ...
$ cd gloseval
$ sbt run
```

**NOTE**: In order to run GLoSEval, MongoDB and RabbitMQ must be running.

### Why can't I use newer versions?

See note [here](https://github.com/synereo/specialk#why-cant-i-use-newer-versions).

## Issues

We welcome reports of any issues on the [issue tracker](https://github.com/synereo/specialk/issues).

We are also using JIRA to track issues for this project and the rest of the Synereo Platform:
https://synereo.atlassian.net/projects/SOC/issues
