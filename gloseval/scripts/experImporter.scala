import com.biosimilarity.evaluator.importerExperimental.Importer
import com.biosimilarity.evaluator.spray.util._

resetMongo()
com.biosimilarity.evaluator.spray.Boot.main(new Array[String](0))
Thread.sleep(3000)  // allow it to 'warm up'
Importer.fromFile()


