move rabbitmq-multi-server.bat into your ...RabbitMQ\rabbitmq_server-2.5.1\sbin directory

run it with 

rabbitmq-multi-server <node short name> <port>

note: It currently disables management plugin (well all plugins) if the port is not 5672