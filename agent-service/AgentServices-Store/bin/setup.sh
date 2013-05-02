#! /bin/sh
sudo rabbitmq-server &
sudo mongod --dbpath /Users/lgm/.mongo/data --port 27017 &