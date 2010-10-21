// -*- mode: Scala;-*- 
// Filename:    Compiler.scala 
// Authors:     lgm                                                    
// Creation:    Mon Aug 30 15:41:06 2010 
// Copyright:   Not supplied 
// Description: 
// ------------------------------------------------------------------------

package com.biosimilarity.lift.model.Eval

import com.biosimilarity.lift.lib._
import com.biosimilarity.lift.model.store._
import com.biosimilarity.lift.model.specialK._
import Absyn._

import scala.xml._
import scala.collection.mutable.Map

import java.net.URI
import java.util.UUID

/* -----------------------------------------------------------------------
 *
 * The compilation strategy is extremely lazy. Code is not compiled
 * until it is actually needed for execution. This is extremely
 * inefficient as a compilation strategy, but works fine as a REPL.
 * 
 * The template for the compilation of an agent is this
 * 
 * object Agent<UUID> extends AgentExecutor {
 *   def execute( tstore : TermStore, reg : Map[Agent,UUID], env : Map[Variation,(Agent,Source)] ) = {
 *     // Agent code goes here
 *   }
 * }
 *
 * So, we have
 *
 * T[| 0 |]( reg : Map[Agent,UUID], env : Map[Variation,(Agent,Source)] ) =
 *   object Agent<UUID> extends AgentExecutor {
 *     def execute( tstore : TermStore ) = {
 *       // Nothing to do
 *     }
 *   }
 * 
 * T[| t?(v)p |]( reg : Map[Agent,UUID], env : Map[Variation,(Agent,Source)] ) =
 *  object Agent<UUID> extends AgentExecutor {
 *    def execute( tstore : TermStore ) = {
 *      val (subst, v) = tstore.get( T[| t |]( reg ) )
 *      val ps = subst( p )
 *      reg( ps ) = getUUID()
 *      T[| ps |]( reg )
 *      ( "Agent" + reg.get( ps ).getOrElse( getUUID() ) ).execute( tstore )
 *    }
 *  }
 * 
 * T[| t!(v)p |]( reg : Map[Agent,UUID], env : Map[Variation,(Agent,Source)] ) =
 *  object Agent<UUID> extends AgentExecutor {
 *    def execute( tstore : TermStore ) = {
 *      store.put( T[| t |]( reg ), T[| v |]( reg ) );
 *      T[| p |]( reg )
 *      ( "Agent" + reg.get( p ).getOrElse( getUUID() )).execute( tstore )
 *    }
 *  }
 * 
 * T[| p | q |]( reg : Map[Agent,UUID], env : Map[Variation,(Agent,Source)] ) =
 *  object Agent<UUID> extends AgentExecutor {
 *    def execute( tstore : TermStore ) = {
 *      object pThread extends Thread {
 *        def run() {
 *          T[| p |]( reg );
 *          ( "Agent" + reg.get( p ).getOrElse( getUUID() )).execute( tstore )
 *        }
 *      }
 *      object qThread extends Thread {
 *        def run() {
 *          T[| q |]( reg );
 *          ( "Agent" + reg.get( q ).getOrElse( getUUID() )).execute( tstore )
 *        }
 *      }
 *      p.run(); q.run()
 *    }
 *  }
 * 
  ----------------------------------------------------------------------- */

trait AgentToScalaXForm {
  type Source
  
  def toScalaSource(
    agentExpr : Agent,
    reg : Map[Agent,UUID],
    env : Map[Variation,(Agent,Option[Source])]
  ) : Source
  def toScalaSource(
    comp : Composition,
    compUnitId : UUID,
    reg : Map[Agent,UUID],
    env : Map[Variation,(Agent,Option[Source])] 
  ) : Source
  def toScalaSource(
    choice : Superposition, 
    compUnitId : UUID,
    reg : Map[Agent,UUID],
    env : Map[Variation,(Agent,Option[Source])]    
  ) : Source
  def toScalaSource(
    out : Excretion,
    compUnitId : UUID,
    reg : Map[Agent,UUID],
    env : Map[Variation,(Agent,Option[Source])]
  ) : Source
  def toScalaSource(
    in : Ingestion,
    compUnitId : UUID,
    reg : Map[Agent,UUID],
    env : Map[Variation,(Agent,Option[Source])]
  ) : Source
  def toScalaSource(
    rep : Replication,
    compUnitId : UUID,
    reg : Map[Agent,UUID],
    env : Map[Variation,(Agent,Option[Source])]
  ) : Source
}

trait Compiler 
extends AgentToScalaXForm
with UUIDOps {
  import scala.collection.JavaConversions._

  type Source = Elem
  type FactualCnxnCtxtLabel =
    CnxnCtxtLabel[String,String,String] with Factual

  def toScalaException(
    ind : Indirection,
    cUI : UUID,
    reg : Map[Agent,UUID],
    env : Map[Variation,(Agent,Option[Source])]
  ) : Source = {
    <source>throw new Exception( "unbound variable" )</source>
  }

  override def toScalaSource(
    agentExpr : Agent,
    reg : Map[Agent,UUID],
    env : Map[Variation,(Agent,Option[Source])]
  ) : Source
  = {
    val compUnitId = getUUID()

    agentExpr match {
      case comp : Composition => {
	toScalaSource( comp, compUnitId, reg, env )
      }
      case choice : Superposition => {
	toScalaSource( choice, compUnitId, reg, env )
      }
      case out : Excretion => {
	toScalaSource( out, compUnitId, reg, env )
      }
      case in : Ingestion => {
	toScalaSource( in, compUnitId, reg, env )
      }
      case rep : Replication => {
	toScalaSource( rep, compUnitId, reg, env )
      }
    }
  }

  override def toScalaSource(
    comp : Composition,
    compUnitId : UUID,
    reg : Map[Agent,UUID],
    env : Map[Variation,(Agent,Option[Source])] 
  ) : Source = {        
    val compId = getUUID()
    reg( comp ) = compId
    val objCompId = "Composition" + compId

    val threadIdMap = new scala.collection.mutable.HashMap[Agent,UUID]()

    for( agent <- comp.listagent_ ) {
      threadIdMap( agent ) = getUUID()
    }

    def agentObjName( agent : Agent ) = {
      "Thread" + threadIdMap.get( agent ).getOrElse( getUUID() )
    }

    val agentThreadObjectDecls =
      <source>{
	for( agent <- comp.listagent_ )
	yield {
	  //println( "compiling : " + agent)
	  val agentSrc = toScalaSource( agent, reg, env ).text
	  val agentThreadSrc =
	    <source>object { agentObjName( agent ) } extends Thread {{ override def run() = { agentSrc } }}</source> //{{ { agentSrc } }} }}</source>
	  agentThreadSrc.text
	}
      }</source>
    val agentThreadObjectRuns =
      <source>{
	for( agent <- comp.listagent_ )
	yield {
	  //println( "generating execution invocation : " + agent)
	  val agentObjectRunSrc =
	    <source>{ agentObjName( agent ) + ".run()" + " ; "}</source>
	  agentObjectRunSrc.text
	}
      }</source>

    val compilationUnitSource =
      <source>{{ object {objCompId} extends AgentExecutor {{ override def execute( tstore : TermStore ) = {{ { if (agentThreadObjectDecls.length > 0) {{ {agentThreadObjectDecls} ++ { List( " ; " ) } ++ {agentThreadObjectRuns} }} } }} }} ; {objCompId}.execute( tstore ) }}</source>

    compilationUnitSource
  }

  override def toScalaSource(
    choice : Superposition, 
    compUnitId : UUID,
    reg : Map[Agent,UUID],
    env : Map[Variation,(Agent,Option[Source])]    
  ) : Source = {
    val choiceId = getUUID()
    reg( choice ) = choiceId
    val objChoiceId = "Choice" + choiceId

    val compilationUnitSource =
      <source>{{ object {objChoiceId} extends AgentExecutor {{ override def execute( tstore : TermStore ) = {{ /* to do */ }} }} ; {objChoiceId}.execute( tstore ) }}</source>

    compilationUnitSource
  }

  def toScalaSource(
    pattern : Pattern,
    compUnitId : UUID,
    reg : Map[Agent,UUID],
    env : Map[Variation,(Agent,Option[Source])]    
  ) : Source = {
    val compUnitId = getUUID()
    pattern match {
      case elem : Element => {
	toScalaSrc( elem, compUnitId, reg, env )
      }
      case vrbl : Variable => {
	toScalaSrc( vrbl, compUnitId, reg, env )
      }
      case ltrl : Literal => {
	toScalaSrc( ltrl, compUnitId, reg, env )
      }
    }    
  }

  def symbolString( elem : Element ) = {
    elem.symbol_ match {
      case tag : Tag => tag.lident_
    }
  }
  
  def toScalaSource(
    lstPtrn : ListPattern,
    compUnitId : UUID,
    reg : Map[Agent,UUID],
    env : Map[Variation,(Agent,Option[Source])]
  ) : Source = {
    <source>{
      for( ptrn <- lstPtrn )
      yield {
	toScalaSource( ptrn, compUnitId, reg, env ).text
      }
    }</source>
  }

  def toScalaSrc(
    vary : Variation,
    compUnitId : UUID,
    reg : Map[Agent,UUID],
    env : Map[Variation,(Agent,Option[Source])]
  ) : Source = {
    <source>{
      vary match {
	case atom : Atomic => {
	  atom.uident_ 
	}
	case transcript : Transcription => {
	  getUUID().toString 
	}
      }
    }</source>
  }
  
  def toScalaSrc(
    elem : Element,
    compUnitId : UUID,
    reg : Map[Agent,UUID],
    env : Map[Variation,(Agent,Option[Source])]
  ) : Source = {
    val elemNSpace = symbolString( elem )
    val elemContents =
      toScalaSource( elem.listpattern_, compUnitId, reg, env )
    <source>new CnxnBranch[String,String]( {elemNSpace}, {elemContents.text} )</source>
  }

  def toScalaSrc(
    vrbl : Variable,
    compUnitId : UUID,
    reg : Map[Agent,UUID],
    env : Map[Variation,(Agent,Option[Source])]
  ) : Source = {
    <source>new CnxnLeaf[String,String]( { toScalaSrc( vrbl.variation_, compUnitId, reg, env ).text } )</source>
  }

  def toScalaSrc(
    ltrl : Literal,
    compUnitId : UUID,
    reg : Map[Agent,UUID],
    env : Map[Variation,(Agent,Option[Source])]
  ) : Source = {
    <source>new CnxnLeaf[String,String]( {
    ltrl match {
      case boollit : BooleanLiteral => {
	boollit.duality_ match {
	  case t : Verity => "true"
	  case f : Absurdity => "false"
	}
      }
      case strlit : StringLiteral => {
	strlit.string_
      }
      case intlit : IntegerLiteral => {
	intlit.integer_
      }
      case dbllit : DoubleLiteral => {
	dbllit.double_
      }
    }
    } )</source>
  }

  def toScalaObject(
    pattern : Pattern,
    compUnitId : UUID,
    reg : Map[Agent,UUID],
    env : Map[Variation,(Agent,Option[Source])]    
  ) : FactualCnxnCtxtLabel = {
    val compUnitId = getUUID()
    pattern match {
      case elem : Element => {
	toScalaObj( elem, compUnitId, reg, env )
      }
      case vrbl : Variable => {
	toScalaObj( vrbl, compUnitId, reg, env )
      }
      case ltrl : Literal => {
	toScalaObj( ltrl, compUnitId, reg, env )
      }
    }    
  }

  def toScalaObj(
    lstPtrn : ListPattern,
    compUnitId : UUID,
    reg : Map[Agent,UUID],
    env : Map[Variation,(Agent,Option[Source])]
  ) : List[FactualCnxnCtxtLabel] = {
    (for( ptrn <- lstPtrn )
    yield {
      toScalaObject( ptrn, compUnitId, reg, env )
    }).toList
  }

  def toScalaObj(
    elem : Element,
    compUnitId : UUID,
    reg : Map[Agent,UUID],
    env : Map[Variation,(Agent,Option[Source])]
  ) : FactualCnxnCtxtLabel = {
    new CnxnCtxtBranch[String,String,String](
      symbolString( elem ),
      toScalaObj( elem.listpattern_, compUnitId, reg, env )
    )
  }

  def toScalaObj(
    vrbl : Variable,
    compUnitId : UUID,
    reg : Map[Agent,UUID],
    env : Map[Variation,(Agent,Option[Source])]
  ) : FactualCnxnCtxtLabel = {
    new CnxnCtxtLeaf[String,String,String](
      (
	vrbl.variation_ match {
	  case atom : Atomic => Right( atom.uident_ )
	  case transcript : Transcription => Right( getUUID().toString )
	}
      )
    )
  }

  def toScalaObj(
    ltrl : Literal,
    compUnitId : UUID,
    reg : Map[Agent,UUID],
    env : Map[Variation,(Agent,Option[Source])]
  ) : FactualCnxnCtxtLabel = {
    new CnxnCtxtLeaf[String,String,String](
      ltrl match {
	case boollit : BooleanLiteral => {
	  boollit.duality_ match {
	    case t : Verity => Left( "true" )
	    case f : Absurdity => Left( "false" )
	  }
	}
	case strlit : StringLiteral => {
	  Left( strlit.string_ )
	}
	case intlit : IntegerLiteral => {
	  Left( intlit.integer_ + "" )
	}
	case dbllit : DoubleLiteral => {
	  Left( dbllit.double_ + "" )
	}
      }
    )
  }

  def toScalaSource(
    info : Information,
    compUnitId : UUID,
    reg : Map[Agent,UUID],
    env : Map[Variation,(Agent,Option[Source])]
  ) : Source = {
    info match {
      case ind : Indirection => {
	toScalaSrc( ind, compUnitId, reg, env )
      }
      case refl : Reflection => {
	toScalaSrc( refl, compUnitId, reg, env )
      }
    }
  }

  def toScalaSrc(
    ind : Indirection,
    compUnitId : UUID,
    reg : Map[Agent,UUID],
    env : Map[Variation,(Agent,Option[Source])]    
  ) : Source = {
    env.get( ind.variation_ ) match {
      case None => {
	toScalaException( ind, compUnitId, reg, env )
      }
      case Some( ( _, Some( src ) ) ) => {
	src
      }
      case Some( ( agent, _ ) ) => {
	toScalaSource( agent, reg, env )
      }
    }
  }

  def toScalaSrc(
    refl : Reflection,
    compUnitId : UUID,
    reg : Map[Agent,UUID],
    env : Map[Variation,(Agent,Option[Source])]
  ) : Source = {
    toScalaSource( refl.agent_, reg, env )
  }

  override def toScalaSource(
    out : Excretion,
    compUnitId : UUID,
    reg : Map[Agent,UUID],
    env : Map[Variation,(Agent,Option[Source])]
  ) : Source = {
    val outId = getUUID()
    reg( out ) = outId
    val objOutId = "OutputGuarded" + outId

    val applicand =
      out.concretion_ match {
	case aplcnd : Applicand => aplcnd
	case _ => throw new Exception( "unexpected concretion type" )
      }
    val information = applicand.information_
    
    val patternSrc =
      toScalaSource( out.pattern_, outId, reg, env )
    val actlSrc =
      toScalaSource( information, outId, reg, env )
    val continuationSrc =
      toScalaSource( applicand.agent_, reg, env )
    val contId =
      reg.get( applicand.agent_ ).getOrElse( getUUID() ).toString

    val outputExecutorSrc =
      <source>tstore.put( {patternSrc.text}, {actlSrc.text} ) ; {continuationSrc.text} </source>
    val compilationUnitSource =
      <source>{{ object {objOutId} extends AgentExecutor {{ override def execute( tstore : TermStore ) = {outputExecutorSrc.text} }} ; {objOutId}.execute( tstore ) }}</source>

    compilationUnitSource
  }

  override def toScalaSource(
    in : Ingestion,
    compUnitId : UUID,
    reg : Map[Agent,UUID],
    env : Map[Variation,(Agent,Option[Source])]
  ) : Source = {
    val applicant =
      in.abstraction_ match {
	case aplcnt : Applicant => aplcnt
	case _ => throw new Exception( "unexpected abstraction type" )
      }
    val inId = getUUID()
    reg( in ) = inId
    val objInId = "InputGuarded" + inId
    
    val optSubstId = getUUID()
    val optSubstVar = "subst" + optSubstId

    val substId = getUUID()
    val substVar = "subst" + substId
    
    val unifyId = getUUID()
    val unifyVar = "unify" + unifyId

    val patternSrc =
      toScalaSource( in.pattern_, inId, reg, env )

    val patternObj =
      toScalaObject( in.pattern_, inId, reg, env )
    val patternVars =
      patternObj.names.map( 
	_ match {
	  case Right( n ) => n
	  case Left( n ) => n
	}
      )
    val substSrc =
      <source>{
	for( pVar <- patternVars )
	yield {
	  val assignment =
	    <source>{pVar} = {substVar}.get( "X{pVar}" ) ; </source> 
	  assignment.text
	}
      }</source>

    val fmlSrc =
      toScalaSrc( applicant.variation_, inId, reg, env )

    val contSrc = toScalaSource( applicant.agent_, reg, env )

    val contId = reg.get( applicant.agent_ ).getOrElse( getUUID() ).toString

    val getKSrc = 
      <source>( {optSubstVar} ) => {{ Some( RBound( Some( {fmlSrc.text} ), {unifyVar} ) ) = {optSubstVar} ; {unifyVar} match {{ case Some( {substVar} ) => {{ {substSrc.text} {contSrc.text} }} case None => {{ {contSrc.text} }} }} }}</source>

    val inputExecutorSrc =
      <source>{{ val {"dummy" + getUUID()} = tstore.get( {patternSrc.text}, {getKSrc.text} ) }}</source>

    val compilationUnitSource =
      <source>{{ object {objInId} extends AgentExecutor {{ override def execute( tstore : TermStore ) = {inputExecutorSrc.text} }} ; {objInId}.execute( tstore ) }}</source>

    compilationUnitSource
  }

  override def toScalaSource(
    rep : Replication,
    compUnitId : UUID,
    reg : Map[Agent,UUID],
    env : Map[Variation,(Agent,Option[Source])]
  ) : Source = {
    val repId = getUUID()
    reg( rep ) = repId

    val objRepId = "Replication" + repId
    val fmlSrc =
      toScalaSrc( rep.variation_, repId, reg, env )
    val compilationUnitSource =
      <source>{{ object {objRepId} extends AgentExecutor {{ override def execute( tstore : TermStore ) = {{ {fmlSrc}.execute( tstore ) }} }} ; {objRepId}.execute( tstore ) }}</source>

    compilationUnitSource
  }
}

object theCompiler extends Compiler {
  def compile( expr : Agent ) = {
    toScalaSource(
      expr, 
      new scala.collection.mutable.HashMap[Agent,UUID](),
      new scala.collection.mutable.HashMap[Variation,(Agent,Option[Source])]()
    )
  }
}
