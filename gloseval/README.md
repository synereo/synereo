GLoSEval
========

Network evaluator for Generalized Language of Streams

Local copy:

To get dependencies up and running:

```
env PATH=/Users/justin/Projects/LivelyGig/mongodb-linux-x86_64-2.6.4/bin/:/Users/justin/Projects/LivelyGig/rabbitmq_server-2.6.1/sbin/:$PATH sudo /bin/sh sudo /bin/sh sudo rabbitmq-server & sudo mongod --dbpath /Users/justin/.mongo/data --port 27017 --smallfiles
```

To start local server:

```
mvn scala:console
load scripts/start.scala
```