# Agent-Service-ATI-IA

[![Join the chat at https://gitter.im/synereo/agent-service-ati-ia](https://badges.gitter.im/synereo/agent-service-ati-ia.svg)](https://gitter.im/synereo/agent-service-ati-ia?utm_source=badge&utm_medium=badge&utm_campaign=pr-badge&utm_content=badge)

Agent Service ATI IA is...

* an agent and privacy model built on [SpecialK](https://github.com/synereo/specialk)
* a DSL interpreter, along with a DSL for:
  * sessions and associated security
  * agents
  * connections between agents, including a self-connection known as an "alias"
  * posts
  * filters (and evaluation for query)
  * poll and subscription, with paging
  * protocols and handlers, including:
    * introduction (of agents)
    * reputation
    * verification

## Installation

To work with the Agent Service, you will need:

* [MongoDB](https://www.mongodb.com/), version 2.6.12 (also tested with version 2.4.14)
  * available at https://www.mongodb.com/download-center (go to "Previous Releases")
* [Erlang](https://www.erlang.org/), version 15B03 (also tested with version R14B04) (required to run RabbitMQ)
  * available at https://www.erlang-solutions.com/resources/download.html
* [RabbitMQ](http://www.rabbitmq.com/), version 3.0.2 (also tested with version 2.7.1)
  * available at http://www.rabbitmq.com/download.html (go to "Older Versions")
* [Memcached](https://memcached.org/)
* Java Development Kit (JDK), version 7
  * available at http://www.oracle.com/technetwork/java/javase/downloads/jdk7-downloads-1880260.html
  * alternatively, the [OpenJDK](http://openjdk.java.net/) can be installed using most common package managers.
* [sbt](http://www.scala-sbt.org/)
  * available at http://www.scala-sbt.org/

After installing these dependencies, you can clone the SpecialK repo and run tests:
```
$ git clone https://github.com/synereo/agent-service-ati-ia.git
  ...
$ cd agent-service-ati-ia/AgentServices-Store
$ sbt test
```

**NOTE**: In order to run these tests, MongoDB, RabbitMQ, and Memcached must be running.

## Usage

Currently, the Agent Service is intended to be used in conjunction with the [SpecialK](https://github.com/synereo/specialk) and [GLoSEval](https://github.com/synereo/gloseval).

## Issues

We welcome reports of any issues on the [issue tracker](https://github.com/synereo/agent-service-ati-ia/issues).

We are also using JIRA to track issues for this project and the rest of the Synereo Platform:
https://synereo.atlassian.net/projects/SOC/issues

## More Information

* Lucius Gregory Meredith's [*Agents and Agency in the Internet*](https://youtu.be/CjSr9Iui1ko) presentation at the Scala Bay Meetup in Fall 2013.
* Synereo [Development Workshops](https://www.youtube.com/playlist?list=PLsMIFzUId4x_FoKGsr_dVvp-v_VQdGc8t) hosted on our [YouTube Channel](https://www.youtube.com/channel/UCU5CBbxAeFYnodf32w3ahOQ)
