import com.biosimilarity.evaluator.importer.Importer
import com.biosimilarity.evaluator.spray.util._

resetMongo()
com.biosimilarity.evaluator.spray.Boot.main(new Array[String](0))
Thread.sleep(3000)  // allow it to 'warm up'
Importer.runTestFiles()


