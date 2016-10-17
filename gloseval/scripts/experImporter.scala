import com.biosimilarity.evaluator.importerExperimental.Importer
//import com.biosimilarity.evaluator.importer.Importer
import com.biosimilarity.evaluator.util._

resetMongo()
com.biosimilarity.evaluator.spray.Boot.main(new Array[String](0))
Thread.sleep(3000)  // allow it to 'warm up'
//Importer.fromTest("zeroToTen")
//Importer.fromTest("singlePost")
Importer.fromTest("20161003c")


