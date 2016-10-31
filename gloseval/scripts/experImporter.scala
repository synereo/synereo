import com.biosimilarity.evaluator.util._
import com.biosimilarity.evaluator.spray.Boot
import com.biosimilarity.evaluator.importer.ExperimentalImporter

resetMongo()
Boot.main(Array.empty[String])
Thread.sleep(3000L)
ExperimentalImporter.fromTestData("zeroToTen")
