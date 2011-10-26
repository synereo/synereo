require('./harness');

var recvCount = 0;
//var body = "hello world";
var body =
    {
	application:
	{
	    operation:
	    {
		abstraction: { formal: "x", body: { mention: "x" } }
	    },
	    actual :
	    {
		abstraction: { formal: "x", body: { mention: "x" } }
	    }
	}
    }

var srcQStr = 'node-direct-src-exchange'
var trgtQStr = 'node-direct-trgt-exchange' 

connection.addListener('ready', function () {
    puts("connected to " + connection.serverProperties.product);

    var queueOpts = { durable: true, autoDelete: false, exclusive: false } 
    
    connection.exchange( srcQStr, {type: 'direct'}, function(exchange) {
	connection.queue( srcQStr + '_queue', queueOpts, function(q) {
            q.bind(exchange, "routeroute")
            q.on('queueBindOk', function() {
		puts("listening for messages...");
		q.subscribe(function (json, headers, deliveryInfo) {
		    recvCount++;		    
		    puts( "received: " + JSON.stringify( json ) )
		})
            });
	});
    });

    connection.exchange( trgtQStr, {type: 'direct'}, function(exchange) {
	connection.queue( trgtQStr + '_queue', queueOpts, function(q) {
            q.bind(exchange, "routeroute")
            q.on('queueBindOk', function() {
		puts("publishing message");
		exchange.publish(
		    "routeroute",
		    body,
		    {contentType: 'application/json'}
		);		
            });
	});
    });
});


process.addListener('exit', function () {
    //assert.equal(1, recvCount);
    puts("exiting...");
});
