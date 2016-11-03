import com.biosimilarity.evaluator.importer.Importer
import scala.util.Try

Importer.fromTestData(Try(args(0)).getOrElse("zeroToTen"))
