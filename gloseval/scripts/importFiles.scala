import com.biosimilarity.evaluator.importer.Importer
import com.biosimilarity.evaluator.spray.util._

resetMongo()
com.biosimilarity.evaluator.spray.Boot.main(Array.empty[String])
Thread.sleep(5000)  // allow it to 'warm up'
Importer.fromFiles()