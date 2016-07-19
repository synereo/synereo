package com.biosimilarity.lift.lib.term.conversion

import org.scalatest.{Matchers, WordSpec}

class CnxnTermSpec extends WordSpec with Matchers {

  import com.biosimilarity.lift.lib.term.Prolog.Absyn._
  import com.biosimilarity.lift.lib.term.Prolog._
  import com.biosimilarity.lift.lib.zipper._
  import com.biosimilarity.lift.model.store._

  case class Identity[T]() extends ((T) => T) {
    override def apply(t: T) = t
  }

  object idS extends Identity[String]

  object CnxnStrZipr
      extends CnxnNavigation[String, String, String]
      with CnxnMutation[String, String, String]
      with CnxnZipperComposition[String, String, String]

  object ContextVar {
    import java.util.UUID
    val thisContextVar: String = "X" + UUID.randomUUID.toString.replace("-", "") + "X"
  }

  case class TermToCCLStr() extends TermToCnxnCtxtLabel(idS, idS, idS, idS, idS, idS, CnxnStrZipr, ContextVar.thisContextVar) {
    def strToTerm(s: String): CnxnCtxtLabel[String, String, String] = {
      val ast = new parser(new Yylex(new java.io.StringReader(s))).pPredicate()
      val loc: Location[Either[String, String]] =
        Location[Either[String, String]](new CnxnCtxtLeaf[String, String, String](Right[String, String](text2v("_"))), Top())
      val ctxt: Option[Location[Either[String, String]]] = Some(loc)
      val xformedTerm = ast match {
        case apred: APred => visit(apred, ctxt)
        case cpred: CPred => visit(cpred, ctxt)
      }
      xformedTerm match {
        case Some(location) => zipr.decontextualize(location)
        case None           => throw new Exception("xform failed: " + ast)
      }
    }
  }

  "An instance of TermToCCLStr()" should {
    "return a CnxnCtxtLabel when given a String" in {
      val t = TermToCCLStr()
      t.strToTerm("quux") shouldBe a[CnxnCtxtLabel[_, _, _]]
    }
  }
}
