# SpecialK

[![Build Status](https://travis-ci.org/synereo/specialk.svg?branch=1.0)](https://travis-ci.org/synereo/specialk)
[![Gitter](https://badges.gitter.im/synereo/specialk.svg)](https://gitter.im/synereo/specialk?utm_source=badge&utm_medium=badge&utm_campaign=pr-badge&utm_content=badge)

SpecialK is...

* a decentralized, distributed storage mechanism
* a pattern language for building decentralized, distributed applications

See [*SpecialK/KVDB – A Pattern Language for the Web*](http://blog.synereo.com/2015/03/17/specialkkvdb-a-pattern-language-for-the-web/) for further explanation.

## Installation

To work with SpecialK you will need:

* [MongoDB](https://www.mongodb.com/), version 2.6.12 (also tested with version 2.4.14)
  * available at https://www.mongodb.com/download-center (go to "Previous Releases")
* [Erlang](https://www.erlang.org/), version 15B03 (also tested with version R14B04) (required to run RabbitMQ)
  * available at https://www.erlang-solutions.com/resources/download.html
* [RabbitMQ](http://www.rabbitmq.com/), version 3.0.2 (also tested with version 2.7.1)
  * available at http://www.rabbitmq.com/download.html (go to "Older Versions")
* Java Development Kit (JDK), version 7
  * available at http://www.oracle.com/technetwork/java/javase/downloads/jdk7-downloads-1880260.html
  * alternatively, the [OpenJDK](http://openjdk.java.net/) can be installed using most common package managers.
  * alternatively, Azul's Zulu OpenJDK is available at https://www.azul.com/products/zulu/
* [sbt](http://www.scala-sbt.org/)
  * available at http://www.scala-sbt.org/

After installing these dependencies, you can clone the SpecialK repo and run tests:
```
$ git clone https://github.com/synereo/specialk.git
  ...
$ cd specialk
$ sbt test
```

**NOTE**: In order to run these tests, MongoDB and RabbitMQ must be running.

### Why can't I use newer versions?

Short answer: it may be possible!

In the case of MongoDB, SpecialK currently uses version 2.6.4 of [Casbah](http://mongodb.github.io/casbah/).  According to the MongoDB [compatibility chart](https://docs.mongodb.com/ecosystem/drivers/driver-compatibility-reference/#scala-driver-compatibility), this version of Casbah is only supported for use with MongoDB 2.4.x, though we are successfully using it with version 2.6.x.

In the case of Erlang & RabbitMQ, we encourage all those motivated to try newer versions, and welcome reports of successes or failures!

In the case of the JDK, we have experienced issues with version 8 related to our use of [prolog4j](https://github.com/espakm/prolog4j).  See [SOC-101](https://synereo.atlassian.net/browse/SOC-101) for more information.

## Usage

Currently, SpecialK is intended to be used in conjunction with the [Agent Service](https://github.com/synereo/agent-service-ati-ia) and [GLoSEval](https://github.com/synereo/gloseval).

However, it can also be used as a standalone key-value store.

The following files provide an example of setting up a single node to store `String` values in MongoDB.
* [`PersistedMonadicKVDBMongoNodeSetup.scala`](src/test/scala/com/biosimilarity/lift/model/store/PersistedMonadicKVDBMongoNodeSetup.scala)
* [`PersistedMonadicKVDBMongoNodeInstance.scala`](src/test/scala/com/biosimilarity/lift/model/store/PersistedMonadicKVDBMongoNodeInstance.scala)

This set of tests may provide additional insight into how such a node may be used, including retrieving values using the Prolog-based query system:
* [`PersistedMonadicKVDBMongoNodeSpec.scala`](src/test/scala/com/biosimilarity/lift/model/store/PersistedMonadicKVDBMongoNodeSpec.scala)

A more complex usage example involving multiple nodes can be found [here](https://github.com/synereo/agent-service-ati-ia/blob/master/AgentServices-Store/src/main/scala/com/protegra_ati/agentservices/store/AgentKVDBMongoNode.scala#L2276-L4689).

## Issues

We welcome reports of any issues on the [issue tracker](https://github.com/synereo/specialk/issues).

We are also using JIRA to track issues for this project and the rest of the Synereo Platform:
https://synereo.atlassian.net/projects/SOC/issues

## More Information

* Lucius Gregory Meredith's [*Agents and Agency in the Internet*](https://youtu.be/CjSr9Iui1ko) presentation at the Scala Bay Meetup in Fall 2013.
* Synereo [Development Workshops](https://www.youtube.com/playlist?list=PLsMIFzUId4x_FoKGsr_dVvp-v_VQdGc8t) hosted on our [YouTube Channel](https://www.youtube.com/channel/UCU5CBbxAeFYnodf32w3ahOQ)
