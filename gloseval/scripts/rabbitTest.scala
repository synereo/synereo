/*
Run this in two hosts and change the source host and target host IP address
and port numbers to point to each other. To run at scala console prompt.
scala> :load rabbitTest.scala
*/

import java.net.URI 
import com.biosimilarity.lift.lib._ 
import com.biosimilarity.lift.lib.usage.AMQPTPSample._ 
import _root_.com.rabbitmq.client.{Channel=>RabbitChan, _} 
                                                                           
val sourceHost1 = new URI( "amqp://guest:guest@192.168.99.100:5672/synereo" )  
val targetHost1 = new URI( "amqp://guest:guest@192.168.99.100:5672/synereo" ) 
                                                                 
setupAndRunTest( true, sourceHost1, targetHost1, "synereo1", true, 10)
