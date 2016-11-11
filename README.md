# synereo

[![Join the chat at https://gitter.im/synereo/synereo](https://badges.gitter.im/synereo/synereo.svg)](https://gitter.im/synereo/synereo?utm_source=badge&utm_medium=badge&utm_campaign=pr-badge&utm_content=badge)
[![Build Status](https://travis-ci.org/synereo/synereo.svg?branch=staging)](https://travis-ci.org/synereo/synereo)
[![Build status](https://ci.appveyor.com/api/projects/status/8hxpx6mwmi5b8g58/branch/staging?svg=true)](https://ci.appveyor.com/project/henrytill/synereo/branch/staging)

### Contents:
* [Instructions for running a Synereo node](#instructions-for-running-a-synereo-node)
* [Instructions for Developers](#instructions-for-developers)

## Instructions for running a Synereo node

#### WARNING

This is **experimental**, pre-release software and should be used **for testing purposes only**.

While the software is in pre-release phase, there is a **high likelihood of data loss** and **features will change without notice**.

### Requirements
* Basic knowledge of [Docker](https://www.docker.com)
  * New users should start [here](https://docs.docker.com/engine/understanding-docker/)
* A machine capable of running [Docker](https://www.docker.com), with at least 4 GB RAM (> 8 GB recommended)
  * For Linux users, installation information is [available here](https://docs.docker.com/engine/installation/linux)
  * For Mac users, system requirements and installation information is [available here](https://docs.docker.com/docker-for-mac/)
  * For Windows users, system requirements and installation information is [available here](https://docs.docker.com/docker-for-windows/)
  * Gmail based email id

### Installation

**Step 1**. If you haven't already done so, install Docker.  See [above](#requirements) for platform-specific information.

**Step 2**. Open a terminal and pull the node image from Docker Hub.
```sh
docker pull synereo/synereo-node
```

### First Time Usage

After the build completes, run the following command (Please update EMAIL_AUTH_USERNAME, EMAIL_AUTH_PASSWORD and EMAIL_FROM_ADDRESS with valid Gmail based email id and this is required. The password is clear text at this moment and use with caution - You may create a new Gmail id for this purpose. For example, if id is "s52.38.13.42@gmail.com" then - EMAIL_AUTH_USERNAME=s52.38.13.42, EMAIL_AUTH_PASSWORD=validpassword and EMAIL_FROM_ADDRESS=s52.38.13.42@gmail.com
):
```sh
docker run -it -p 443:9876 -h mynodehost --dns 8.8.8.8 -e EMAIL_AUTH_USERNAME=[change_me] -e EMAIL_AUTH_PASSWORD=[change_me] -e EMAIL_FROM_ADDRESS=[chamge_metoo]@gmail.com -d synereo/synereo-node --name synereo-node-01
```

**NOTE**:

The names `mynodehost` and `synereo-node-01` are arbitrary. You may use whatever name you prefer for these settings and refer them accordingly if needed.

### Accessing the Synereo Social Platform

To access the application, you must first know the IP address of your running container.

OS|Docker Version |Default IP Address
--------|--------|--------
Mac OSX| < 1.12 | `192.168.99.100`
Mac OSX| > 1.12 | `127.0.0.1` (aka `localhost`)
Windows 10| - | `192.168.99.100` (aka `locahost`)
Linux | - | `172.17.0.1`

Alternatively, on Linux and Windows, you can get the IP address of your running container using the following command (this IP may not be used to access the application):
```sh
docker inspect --format '{{ .NetworkSettings.IPAddress }}' synereo-node-01
```

You can then access the application with your web browser at:
```
https://<Deafult IP Address>/
```
For example: https://172.17.0.1/ or https://localhost/ or https://192.168.99.100/ depending upon underlying OS.

**NOTE**: When accessing the application for the first time, your browser will warn you that the site is insecure.  This happens because the pre-release version of this software uses a self-signed TLS certificate.  You should follow your browser's instructions about approving the site's certificate.

**Congratulations!**, you can now log in using the administrator account:

Username|Password
--------|--------
admin@localhost|a

Or follow instructions to register as a new user [here](https://github.com/synereo/docs/wiki/Registering-as-a-new-user)

**NOTE**: This username and password can be changed by editing the `eval.conf` file inside the running container.

### Stopping the Container

To stop the container:

```sh
docker stop synereo-node-01
```

### General Usage

To restart the container:

```sh
docker start synereo-node-01
```

### Further Help

Please visit the `#docker-testing` channel on [our Slack](https://slack.synereo.com).

## Instructions for Developers

### Requirements

To work with the projects in this repository you will need:
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

To build a Docker image you will need:
* [Docker](https://www.docker.com/)
  * available at https://www.docker.com/products/docker

Additionally, to run the Agent Service test suites you will need:
* [Memcached](https://memcached.org/), latest stable version
  * available at https://memcached.org/downloads

### Usage

After installing the these dependencies, you can clone this repo and run tasks using sbt:
```
$ git clone https://github.com/synereo/synereo.git
  ...
$ cd synereo
$ sbt "gloseval/run gencert --self-signed"
  ...

# Run a GLoSEval server
$ sbt gloseval/run
  ...

# Run the SpecialK test suites
$ sbt specialk/test
  ...

# Run the test suites for all projects
$ sbt test
  ...

# Build a Docker image
$ sbt gloseval/docker:publishLocal
  ...
```

**NOTE**: In order to run most tasks, MongoDB and RabbitMQ must also be running.

For the Agent Service test suites, Memcached must also be running.

### Issues

We welcome reports of any issues on the [issue tracker](https://github.com/synereo/synereo/issues).

We are also using JIRA to track issues for this project and the rest of the Synereo Platform:
https://synereo.atlassian.net/projects/SOC/issues
