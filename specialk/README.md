# SpecialK

SpecialK is...

* a decentralized, distributed storage mechanism
* a pattern language for building decentralized, distributed applications

See [*SpecialK/KVDB – A Pattern Language for the Web*](http://blog.synereo.com/2015/03/17/specialkkvdb-a-pattern-language-for-the-web/) for further explanation.

## Requirements

To work with SpecialK you will need:

* [MongoDB](https://www.mongodb.com/), version 2.6.12 (also tested with version 2.4.14, 3.2.9)
  * available at https://www.mongodb.com/download-center (go to "Previous Releases")
* [Erlang](https://www.erlang.org/), version 15B03 (also tested with version R14B04) (required to run RabbitMQ)
  * available at https://www.erlang-solutions.com/resources/download.html
* [RabbitMQ](http://www.rabbitmq.com/), version 3.0.2 (also tested with version 2.7.1)
  * available at http://www.rabbitmq.com/download.html (go to "Older Versions")
* Java Development Kit (JDK), version 8 (also tested with version 7)
  * available at http://www.oracle.com/technetwork/java/javase/downloads/jdk8-downloads-2133151.html
  * alternatively, the [OpenJDK](http://openjdk.java.net/) can be installed using most common package managers.
* [sbt](http://www.scala-sbt.org/)
  * available at http://www.scala-sbt.org/download.html

## Usage

Currently, SpecialK is intended to be used in conjunction with the [Agent Service](../agent-service) and [GLoSEval](../gloseval).

However, it can also be used as a standalone key-value store.

The following files provide an example of setting up a single node to store `String` values in MongoDB.
* [`PersistedMonadicKVDBMongoNodeSetup.scala`](src/test/scala/com/biosimilarity/lift/model/store/PersistedMonadicKVDBMongoNodeSetup.scala)
* [`PersistedMonadicKVDBMongoNodeInstance.scala`](src/test/scala/com/biosimilarity/lift/model/store/PersistedMonadicKVDBMongoNodeInstance.scala)

This set of tests may provide additional insight into how such a node may be used, including retrieving values using the Prolog-based query system:
* [`PersistedMonadicKVDBMongoNodeSpec.scala`](src/test/scala/com/biosimilarity/lift/model/store/PersistedMonadicKVDBMongoNodeSpec.scala)

A more complex usage example involving multiple nodes can be found [here](../agent-service/AgentServices-Store/src/main/scala/com/protegra_ati/agentservices/store/AgentKVDBMongoNode.scala#L2276-L4689).

## Issues

We welcome reports of any issues on the [issue tracker](https://github.com/synereo/synereo/issues).

We are also using JIRA to track issues for this project and the rest of the Synereo Platform:
https://synereo.atlassian.net/projects/SOC/issues

## More Information

* Lucius Gregory Meredith's [*Agents and Agency in the Internet*](https://youtu.be/CjSr9Iui1ko) presentation at the Scala Bay Meetup in Fall 2013.
* Synereo [Development Workshops](https://www.youtube.com/playlist?list=PLsMIFzUId4x_FoKGsr_dVvp-v_VQdGc8t) hosted on our [YouTube Channel](https://www.youtube.com/channel/UCU5CBbxAeFYnodf32w3ahOQ)
