import com.biosimilarity.evaluator.importer.Importer
import com.biosimilarity.evaluator.spray.Boot
import com.biosimilarity.evaluator.spray.util._

resetMongo()
Boot.main(new Array[String](0))
Thread.sleep(3000L)
Importer.fromTestData()
