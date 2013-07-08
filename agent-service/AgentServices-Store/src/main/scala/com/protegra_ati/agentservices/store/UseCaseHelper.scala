// -*- mode: Scala;-*- 
// Filename:    UseCaseHelper.scala 
// Authors:     lgm                                                    
// Creation:    Mon May 13 13:57:26 2013 
// Copyright:   Not supplied 
// Description: 
// ------------------------------------------------------------------------

package com.biosimilarity.evaluator.distribution

import com.protegra_ati.agentservices.store._
import com.biosimilarity.lift.model.store._

import scala.util.continuations._ 
import scala.collection.mutable.HashMap

import java.util.UUID

trait ChannelGeneration {
  def erql( sessionId : String = UUID.randomUUID().toString ) : ( String, CnxnCtxtLabel[String,String,String] ) = {
    (
      sessionId,
      DSLCommLinkCtor.ExchangeLabels.evalRequestLabel()( sessionId.toString ).getOrElse( 
	throw new Exception( "error making evalRequestLabel" )
      )
    )
  }
  def erspl( sessionId : String = "_" ) : ( String, CnxnCtxtLabel[String,String,String] ) = {
    (
      sessionId,
      DSLCommLinkCtor.ExchangeLabels.evalResponseLabel()( sessionId ).getOrElse( 
	throw new Exception( "error making evalRequestLabel" )
      )
    )
  }
}

trait FuzzyStreams {
  def uuidTuple( l : Int ) : List[UUID] = {
    ( List[UUID]( ) /: ( 1 to l ) )( 
      {
        ( acc, e ) => acc ++ List( UUID.randomUUID )
      }
    )
  }
  def tStream[T]( seed : T )( fresh : T => T ) : Stream[T] = {
    lazy val loopStrm : Stream[T] =
      ( List( seed ) ).toStream append ( loopStrm map fresh );
    loopStrm
  }      
  def uuidStream() : Stream[UUID] =
    tStream[UUID]( UUID.randomUUID )(
      {
        ( uuid : UUID ) => {
          UUID.randomUUID
        }
      }
    )
  def mkUuidStreamStream() : Stream[Stream[UUID]] =
    tStream[Stream[UUID]]( uuidStream() )(
      {
        ( uuidStrm : Stream[UUID] ) => {
          uuidStream()
        }
      }
    )
  @transient
  lazy val uuidStreamStream : Stream[Stream[UUID]] =
    mkUuidStreamStream()  
}

trait FuzzyTerms {
  self : FuzzyStreams =>
  def randomGroundTerm(
    rndm : scala.util.Random = new scala.util.Random()
  ) : String = {
    val termType = rndm.nextInt( 3 )
    termType match {
      case 0 => ( rndm.nextInt( 2 ) > 0 ).toString
      case 1 => rndm.nextInt( Int.MaxValue ).toString
      //case 2 => rndm.nextInt( Int.MaxValue ).toFloat.toString
      case 2 => "\"" + UUID.randomUUID().toString + "\""
    }
  }

  def randomLabelStr(
    uuidStrm : Stream[UUID] = uuidStream(),
    prefix : String = "label",
    maxBredth : Int = 5,
    maxDepth : Int = 5,
    truncate : Int = 10,
    streamPrefix : Int = 1000
  ) : String = {
    val rndm = new scala.util.Random()
    if ( maxBredth > 0 ) {        
      val bredth = rndm.nextInt( maxBredth ) + 1
      val functorLocation = rndm.nextInt( streamPrefix )
      val functor = prefix + uuidStrm( functorLocation ).toString.replace( "-", "" ).substring( 0, truncate )
      val subterms =
        if ( bredth > 1 ) {
          ( randomLabelStr( uuidStrm, prefix, maxBredth - 1, maxDepth - 1 ).toString /: ( 2 to bredth ) )(
            {
              ( acc, e ) => {
                acc + "," + randomLabelStr( uuidStrm, prefix, maxBredth - 1, maxDepth - 1 ).toString
              }
            }
          )
        }
        else {
          randomLabelStr( uuidStrm, prefix, maxBredth - 1, maxDepth - 1 ).toString
        }
      functor + "(" + subterms + ")"
    } else {
      randomGroundTerm( rndm )
    }
  }  
}

trait FuzzyTermStreams {
  self : CnxnString[String,String,String] with FuzzyTerms with FuzzyStreams =>
    import com.protegra_ati.agentservices.store.extensions.StringExtensions._
  def mkEvalRequestLabelStream() : Stream[CnxnCtxtLabel[String,String,String]] = {
    uuidStreamStream.take( 1 )( 0 ).map(
      ( uuid : UUID ) => {
        DSLCommLinkCtor.ExchangeLabels.evalRequestLabel()( uuid.toString ).getOrElse( 
          throw new Exception( "error making evalRequestLabel" )
        )
      }
    )
  }
  @transient
  lazy val evalRequestLabelStream : Stream[CnxnCtxtLabel[String,String,String]] = {
    mkEvalRequestLabelStream()
  }
  def mkRandomLabelStringStream(
    uuidStrmStrm : Stream[Stream[UUID]] = mkUuidStreamStream()
  ) : Stream[String] = {
    uuidStrmStrm.map( randomLabelStr( _ ) )
  }
  def mkRandomLabelStream() : Stream[CnxnCtxtLabel[String,String,String]] = {
    mkRandomLabelStringStream().map(
      fromTermString( _ ).getOrElse(
        throw new Exception( "unable to parse label string" )
      )
    )
  }
  @transient
  lazy val randomLabelStream : Stream[CnxnCtxtLabel[String,String,String]] = {
    mkRandomLabelStream()
  }
  def labelTuple(
    l : Int,
    lssMaxPos : Int = ( Int.MaxValue / 100000000 ),
    rndm : scala.util.Random = new scala.util.Random(),
    optlss : Option[Stream[String]] = None,
    labelStrStream : Stream[String] = mkRandomLabelStringStream()    
  ) : List[String] = {
    val lss : Stream[String] = 
      optlss match {
        case None => labelStrStream.take( lssMaxPos + 1 )
        case Some( l ) => l
      }

    ( List[String](  ) /: ( 1 to l ) )( 
      {
        ( acc, e ) => acc ++ List[String]( lss( rndm.nextInt( lssMaxPos ) ) )
      }
    )
  }
  def mkRandomLabelStringTupleStream(
    maxLabels : Int = 10,
    lssMaxPos : Int = ( Int.MaxValue / 100000000 ),
    rndm : scala.util.Random = new scala.util.Random(),
    labelStrStrm : Stream[String] = mkRandomLabelStringStream()
  ) : Stream[List[String]] = {    
    val numLabels : Int = rndm.nextInt( maxLabels ) + 2
    val lss = labelStrStrm.take( lssMaxPos + 1 )
    
    tStream[List[String]]( 
      labelTuple( numLabels, lssMaxPos, rndm, Some( lss ), labelStrStrm )
    )(
      ( seed : List[String] ) => {
        labelTuple( numLabels, lssMaxPos, rndm, Some( lss ), labelStrStrm )
      }
    )      
  }
  def mkRandomLabelTupleStream() : Stream[List[CnxnCtxtLabel[String,String,String]]] = {
    mkRandomLabelStringTupleStream().map(
      { 
        ( labelStrTpl : List[String] ) => {
          labelStrTpl.map(
            fromTermString( _ ).getOrElse(
              throw new Exception( "unable to parse label string" )
            )
          )
        }
      }
    )
  }
  @transient
  lazy val randomLabelTupleStream : Stream[List[CnxnCtxtLabel[String,String,String]]] = {
    mkRandomLabelTupleStream()
  }
  def mkSelfCnxnStream(
    rndm : scala.util.Random = new scala.util.Random(),
    labelStrStrm : Stream[String] = mkRandomLabelStringStream()
  ) : Stream[ConcreteHL.PortableAgentCnxn] = {
    val pos1 : Int = rndm.nextInt( Int.MaxValue / 1000000 ) + 2
    val strmStrm : Stream[Stream[UUID]] = uuidStreamStream.take( pos1 )
    val strm : Stream[UUID] = strmStrm( pos1 - 1 )

    strm.zip( labelStrStrm ).map(
      ( uuidLabelPair : ( UUID, String ) ) => {
        val uri = uuidLabelPair._1.toString.toURI
        val lbl = uuidLabelPair._2
        ConcreteHL.PortableAgentCnxn( uri, lbl, uri ) 
      }
    )
  }
  @transient
  lazy val selfCnxnStream : Stream[ConcreteHL.PortableAgentCnxn] = {
    mkSelfCnxnStream()
  }
  def mkRandomCnxnStream(
    rndm : scala.util.Random = new scala.util.Random(),
    labelStrStrm : Stream[String] = mkRandomLabelStringStream()
  ) : Stream[ConcreteHL.PortableAgentCnxn] = {
    val pos1 : Int = rndm.nextInt( Int.MaxValue / 1000000 ) + 2
    val strmStrm : Stream[Stream[UUID]] = uuidStreamStream.take( pos1 )
    val strmL : Stream[UUID] = strmStrm( pos1 - 1 )
    val strmR : Stream[UUID] = strmStrm( pos1 - 2 )   
    
    strmL.zip( labelStrStrm.zip( strmR ) ).map(
      ( uuidPair : ( UUID, ( String, UUID ) ) ) => {
        ConcreteHL.PortableAgentCnxn(
          uuidPair._1.toString.toURI,
          uuidPair._2._1,
          uuidPair._2._2.toString.toURI
        ) 
      }
    )
  }
  @transient
  lazy val randomCnxnStream : Stream[ConcreteHL.PortableAgentCnxn] = {
    mkRandomCnxnStream()
  }
  def mkRandomCnxnTupleStream(
    maxCnxns : Int = 10,    
    lssMaxPos : Int = ( Int.MaxValue / 100000000 ),
    rndm : scala.util.Random = new scala.util.Random(),
    labelStrStrm : Stream[String] = mkRandomLabelStringStream()
  ) : Stream[List[ConcreteHL.PortableAgentCnxn]] = {    
    val numCnxns : Int = rndm.nextInt( maxCnxns ) + 2
    val pos1 : Int = rndm.nextInt( Int.MaxValue / 1000000 ) + numCnxns + 2
    val strmStrm : Stream[Stream[UUID]] = uuidStreamStream.take( pos1 )   
    val strmL : Stream[UUID] = strmStrm( pos1 - 1 )
    val lss : Stream[String] = labelStrStrm.take( lssMaxPos + 1 )

    val tupleStrm : Stream[List[( String, UUID )]] = 
      tStream[List[( String, UUID )]]( 
        ( labelTuple( numCnxns - 1, lssMaxPos, rndm, Some( lss ) ).zip( uuidTuple( numCnxns - 1 ) ) )
      )(
        ( seed : List[( String, UUID )] ) => {
          ( labelTuple( numCnxns - 1, lssMaxPos, rndm, Some( lss ) ).zip( uuidTuple( numCnxns - 1 ) ) )
        }
      )
      
    strmL.zip( tupleStrm ).map(
      ( uuidPair : ( UUID, List[( String, UUID )] ) ) => {
        ( List[ConcreteHL.PortableAgentCnxn]() /: uuidPair._2 )(
          {
            ( acc, e ) => {
              acc ++ (
                List[ConcreteHL.PortableAgentCnxn](
                  ConcreteHL.PortableAgentCnxn(
                    uuidPair._1.toString.toURI,
                    e._1,
                    e._2.toString.toURI
                  ) 
                )
              )
            }
          }
        )        
      }
    )
  }
  @transient
  lazy val randomCnxnTupleStream : Stream[List[ConcreteHL.PortableAgentCnxn]] = {
    mkRandomCnxnTupleStream()
  }
}

trait MessageGeneration {
  self : CnxnString[String,String,String] =>
    import com.protegra_ati.agentservices.store.extensions.StringExtensions._

  def defaultLabelStr = "myLife( inTheBush( ofGhosts( true ) ) )"
  def mkFeedExpr( labelStr : String = defaultLabelStr ) : ConcreteHL.FeedExpr = {
    val feedLabelStr : String = labelStr
    val feedLabel =
      fromTermString(
	feedLabelStr
      ).getOrElse(
	throw new Exception( "failed to parse feed label" + feedLabelStr )
      )
    val feedCnxn =
      ConcreteHL.PortableAgentCnxn("Jerry.Seinfeld".toURI, "", "Jerry.Seinfeld".toURI) 

    ConcreteHL.FeedExpr( feedLabel, List( feedCnxn ) )      
  }
  def mkScoreExpr( labelStr : String = defaultLabelStr ) : ConcreteHL.ScoreExpr = {
    val scoreLabelStr : String = labelStr
    val scoreLabel =
      fromTermString(
	scoreLabelStr
      ).getOrElse(
	throw new Exception( "failed to parse score label" + scoreLabelStr )
      )
    val scoreCnxn =
      ConcreteHL.PortableAgentCnxn("Jerry.Seinfeld".toURI, "", "Jerry.Seinfeld".toURI) 

    ConcreteHL.ScoreExpr(
      scoreLabel,
      List( scoreCnxn ),
      Left[Seq[ConcreteHL.Cnxn],Seq[ConcreteHL.Label]]( List( scoreCnxn ) )
    )          
  }
  def mkPostExpr( labelStr : String = defaultLabelStr ) : ConcreteHL.InsertContent[String] = {
    val postLabelStr : String = labelStr
    val postLabel =
      fromTermString( 
	postLabelStr
      ).getOrElse(
	throw new Exception( "failed to parse post label" + postLabelStr )
      )
    val postCnxn =
      ConcreteHL.PortableAgentCnxn("Jerry.Seinfeld".toURI, "", "Jerry.Seinfeld".toURI) 
    ConcreteHL.InsertContent[String](
      postLabel,
      List( postCnxn ),
      "David Byrne"
    )          
  }    
}

trait FuzzyMessageStreams {
  self : CnxnString[String,String,String]
          with FuzzyTerms with FuzzyStreams
          with FuzzyTermStreams =>
            import com.protegra_ati.agentservices.store.extensions.StringExtensions._
  def mkFeedExprStream(
    maxCnxns : Int = 10,
    rndm : scala.util.Random = new scala.util.Random(),
    labelStrm : Stream[CnxnCtxtLabel[String,String,String]] = mkRandomLabelStream()
  ) : Stream[ConcreteHL.FeedExpr] = {
    val numCnxns : Int = rndm.nextInt( maxCnxns ) + 1
    val cnxnStrm : Stream[List[ConcreteHL.PortableAgentCnxn]] = mkRandomCnxnTupleStream()
    labelStrm.zip( cnxnStrm ).map(
      { 
        ( lblCnxnPair : ( CnxnCtxtLabel[String,String,String], List[ConcreteHL.PortableAgentCnxn] ) ) => {
          ConcreteHL.FeedExpr(
            lblCnxnPair._1,
            lblCnxnPair._2
          )
        }
      }
    )
  }
  @transient
  lazy val feedExprStream : Stream[ConcreteHL.FeedExpr] = {
    mkFeedExprStream()
  }
  def mkScoreExprStream(
    maxCnxns : Int = 10,
    rndm : scala.util.Random = new scala.util.Random(),
    labelStrm : Stream[CnxnCtxtLabel[String,String,String]] = mkRandomLabelStream()
  ) : Stream[ConcreteHL.ScoreExpr] = {
    val numCnxns : Int = rndm.nextInt( maxCnxns ) + 1
    val cnxnStrm : Stream[List[ConcreteHL.PortableAgentCnxn]] =
      mkRandomCnxnTupleStream()
    val lblStrm : Stream[List[CnxnCtxtLabel[String,String,String]]] =
      mkRandomLabelTupleStream()
    val cnxnOrLabelStrm : Stream[Either[Seq[ConcreteHL.Cnxn],Seq[ConcreteHL.Label]]] =
      cnxnStrm.zip( lblStrm ).map( 
        {
          ( cnxnLblPair : ( List[ConcreteHL.PortableAgentCnxn], List[CnxnCtxtLabel[String,String,String]] ) ) => {
            if ( ( rndm.nextInt( 2 ) > 1 ) ) {
              Left[Seq[ConcreteHL.Cnxn],Seq[ConcreteHL.Label]]( cnxnLblPair._1 )
            }
            else {
              Right[Seq[ConcreteHL.Cnxn],Seq[ConcreteHL.Label]]( cnxnLblPair._2 )
            }
          }
        }
      )

    labelStrm.zip( cnxnStrm.zip( cnxnOrLabelStrm ) ).map(
      { 
        ( lblCnxnPair : ( CnxnCtxtLabel[String,String,String], ( List[ConcreteHL.PortableAgentCnxn], Either[Seq[ConcreteHL.Cnxn],Seq[ConcreteHL.Label]] ) ) ) => {
          ConcreteHL.ScoreExpr(
            lblCnxnPair._1,
            lblCnxnPair._2._1,
            lblCnxnPair._2._2
          )
        }
      }
    )
  }
  @transient
  lazy val scoreExprStream : Stream[ConcreteHL.ScoreExpr] = {
    mkScoreExprStream()
  }
  def mkPostExprStream(
    maxCnxns : Int = 10,
    rndm : scala.util.Random = new scala.util.Random(),
    labelStrm : Stream[CnxnCtxtLabel[String,String,String]] = mkRandomLabelStream(),    
    uuidStrm : Stream[UUID] = uuidStream()
  ) : Stream[ConcreteHL.InsertContent[String]] = {
    val numCnxns : Int = rndm.nextInt( maxCnxns ) + 1
    val cnxnStrm : Stream[List[ConcreteHL.PortableAgentCnxn]] = mkRandomCnxnTupleStream()
    labelStrm.zip( cnxnStrm.zip( uuidStrm ) ).map(
      { 
        ( lblCnxnPair : ( CnxnCtxtLabel[String,String,String], ( List[ConcreteHL.PortableAgentCnxn], UUID ) ) ) => {
          ConcreteHL.InsertContent(
            lblCnxnPair._1,
            lblCnxnPair._2._1,
            lblCnxnPair._2._2.toString
          )
        }
      }
    )
  }
  @transient
  lazy val postExprStream : Stream[ConcreteHL.InsertContent[String]] = {
    mkPostExprStream()
  }
}

object CommManagement {
  import DSLCommLinkCtor._
  @transient
  //lazy val ( client1, server1 ) = stdBiLink()
  var _commLink : Option[StdEvaluationRequestChannel] = None
  def commLink(
    flip : Boolean = false
  ) : StdEvaluationRequestChannel = {
    _commLink match {
      case Some( cLink ) => cLink
      case None => {
	val cLink : StdEvaluationRequestChannel = stdLink()( flip )
	_commLink = Some( cLink )
	cLink
      }
    }
  }
}

trait StorageManagement {
  import CommManagement._
  def doDrop() = {
    import com.biosimilarity.lift.model.store.mongo._
    val clntSess1 =
      MongoClientPool.client( commLink().cache.sessionURIFromConfiguration )
    val mcExecLocal =
      clntSess1.getDB( commLink().cache.defaultDB )( "DSLExecProtocolLocal" )
    val mcExecRemote =
      clntSess1.getDB( commLink().cache.defaultDB )( "DSLExecProtocolRemote" )
    val mcExec =
      clntSess1.getDB( commLink().cache.defaultDB )( "DSLExecProtocol" )

    mcExecLocal.drop
    mcExecRemote.drop
    mcExec.drop
  }
}

trait ExerciseHLDSL {  
  self : ChannelGeneration with MessageGeneration with AgentCnxnTypes =>
  import CommManagement._
  import DSLCommLinkCtor._

  @transient
  val sessionMap =
    new HashMap[String,( Either[ConcreteHL.HLExpr,ConcreteHL.HLExpr], Option[ConcreteHL.HLExpr] )]()

  implicit def toAgentCnxn( pAC : ConcreteHL.PortableAgentCnxn ) : AgentCnxn = {
    new AgentCnxn( pAC.src, pAC.label, pAC.trgt )
  }     
  def doPutBottomRequest( sessionId : String = UUID.randomUUID.toString() ) = {
    val ( _, erqlChan ) = erql( sessionId ) 

    sessionMap += ( sessionId -> ( Left[ConcreteHL.HLExpr,ConcreteHL.HLExpr]( ConcreteHL.Bottom ), None ) )

    reset {      
      commLink().put( erqlChan, DSLCommLink.mTT.Ground( ConcreteHL.Bottom ) )
    }
  }
  def doPutHLExprRequest(
    node : StdEvaluationRequestChannel,
    sessionId : String,
    expr : ConcreteHL.HLExpr
  ) = {
    val ( _, erqlChan ) = erql( sessionId ) 

    sessionMap += ( sessionId -> ( Left[ConcreteHL.HLExpr,ConcreteHL.HLExpr]( expr ), None ) )       

    reset { node.put( erqlChan, DSLCommLink.mTT.Ground( expr ) ) }
  }
  def doPutFeedRequest(
    node : StdEvaluationRequestChannel = commLink(),
    labelStr : String = "myLife( inTheBush( ofGhosts( true ) ) )",
    sessionId : String = UUID.randomUUID.toString()
  ) = {
    doPutHLExprRequest( node, sessionId, mkFeedExpr( labelStr ) )
  }
  def doPutScoreRequest(
    node : StdEvaluationRequestChannel = commLink(),
    labelStr : String = "myLife( inTheBush( ofGhosts( true ) ) )",
    sessionId : String = UUID.randomUUID.toString()
  ) = {
    doPutHLExprRequest( node, sessionId, mkScoreExpr( labelStr ) )
  }
  def doPutPostRequest(
    node : StdEvaluationRequestChannel = commLink(),
    labelStr : String = "myLife( inTheBush( ofGhosts( true ) ) )",
    sessionId : String = UUID.randomUUID.toString()
  ) = {
    doPutHLExprRequest( node, sessionId, mkPostExpr( labelStr ) )
  }

  def doGetRequest( 
    sessionId : String = "SessionId",
    node : StdEvaluationRequestChannel = commLink()
  ) = {
    val ( _, erqlChan ) = erql( sessionId ) 

    reset {
      for( e <- node.subscribe( erqlChan ) ) {
	println( e )
      }
    }
  }
  
  def doGetFeedResponse(
    node : StdEvaluationRequestChannel = commLink(),
    sessionId : String = "SessionId"
  ) = {
    for( eitherExpr <- sessionMap.get( sessionId ) ) {
      eitherExpr match {
	case ( Left( expr@ConcreteHL.FeedExpr( label, cnxns ) ), None ) => {
	  val ( _, ersplChan ) = erspl( sessionId ) 
	  reset {
	    for( e <- node.subscribe( ersplChan ) ) {
	      val rslt = ( Right[ConcreteHL.HLExpr,ConcreteHL.HLExpr]( expr ), None )
	      sessionMap += ( sessionId -> rslt );
	      ()
	    }
	  }
	}
	case ( Left( expr ), _ ) => {
	  throw new Exception( "unexpected expression type: " + expr )
	}
	case ( Right( expr ), _ ) => {
	  println( "session closed" )
	}
      }      
    }
  }
  def doGetScoreResponse(
    node : StdEvaluationRequestChannel = commLink(),
    sessionId : String = "SessionId" 
  ) = {    
    for( eitherExpr <- sessionMap.get( sessionId ) ) {
      eitherExpr match {
	case ( Left( expr@ConcreteHL.ScoreExpr( label, cnxns, staff ) ), None ) => {
	  val ( _, ersplChan ) = erspl( sessionId ) 
	  reset {
	    for( e <- node.subscribe( ersplChan ) ) {
	      val rslt = ( Right[ConcreteHL.HLExpr,ConcreteHL.HLExpr]( expr ), None )
	      sessionMap += ( sessionId -> rslt );
	      ()
	    }
	  }
	}
	case ( Left( expr ), _ ) => {
	  throw new Exception( "unexpected expression type: " + expr )
	}
	case ( Right( expr ), _ ) => {
	  println( "session closed" )
	}
      }      
    }
  }
  def doGetPostResponse(
    node : StdEvaluationRequestChannel = commLink(),
    sessionId : String = "SessionId"
  ) = {
    for( eitherExpr <- sessionMap.get( sessionId ) ) {
      eitherExpr match {
	case ( Left( expr@ConcreteHL.InsertContent( label, cnxns, content ) ), None ) => {
	  val ( _, ersplChan ) = erspl( sessionId ) 
	  reset {
	    for( e <- node.subscribe( ersplChan ) ) {
	      val rslt = ( Right[ConcreteHL.HLExpr,ConcreteHL.HLExpr]( expr ), None )
	      sessionMap += ( sessionId -> rslt );
	      ()
	    }
	  }
	}
	case ( Left( expr ), _ ) => {
	  throw new Exception( "unexpected expression type: " + expr )
	}
	case ( Right( expr ), _ ) => {
	  println( "session closed" )
	}
      }      
    }
  }
}

trait UseCaseHelper extends MessageGeneration
 with ChannelGeneration
 with StorageManagement
 with ExerciseHLDSL
 with AgentCnxnTypes
 with CnxnString[String,String,String] {   
}

package usage {
  object SimpleClient
    extends EvaluationCommsService  
     with MessageGeneration
     with ChannelGeneration
     with EvalConfig
     with DSLCommLinkConfiguration
     with CnxnString[String,String,String]
     with StorageManagement
     with Serializable
  {
    import com.protegra_ati.agentservices.store.extensions.StringExtensions._
  }
  
  object HLDSLProbe
    extends ExerciseHLDSL
     with ChannelGeneration
     with MessageGeneration
     with AgentCnxnTypes
     with CnxnString[String,String,String]
     with Serializable

  object StreamBasedClient
  extends EvaluationCommsService  
  with ChannelGeneration with EvalConfig with DSLCommLinkConfiguration     
  with FuzzyTerms with FuzzyStreams with FuzzyTermStreams with FuzzyMessageStreams
  with CnxnString[String,String,String] with Serializable {
    def doSomeInserts(
      postExprStrm : Stream[ConcreteHL.InsertContent[String]] = mkPostExprStream(),
      maxPosts : Int = 1000,
      minPosts : Int = 1,
      rndm : scala.util.Random = new scala.util.Random()
    ) : Unit = {
      val numPosts =
        scala.math.min( rndm.nextInt( maxPosts ) + 1, minPosts )
      val sessionID = UUID.randomUUID
      val erql = agentMgr().erql( sessionID )
      val erspl = agentMgr().erspl( sessionID )
      for(
        ConcreteHL.InsertContent( filter, cnxns, content : String ) <- postExprStrm.take( numPosts )
      ) {
        agentMgr().post[String]( erql, erspl )( filter, cnxns, content )
      }
    }
    def doSomeFeeds(
      feedExprStrm : Stream[ConcreteHL.FeedExpr] = mkFeedExprStream(),
      maxFeeds : Int = 1000,
      minFeeds : Int = 1,
      rndm : scala.util.Random = new scala.util.Random()
    ) : Unit = {
      val numFeedExprs =
        scala.math.min( rndm.nextInt( maxFeeds ) + 1, minFeeds )
      val sessionID = UUID.randomUUID
      val erql = agentMgr().erql( sessionID )
      val erspl = agentMgr().erspl( sessionID )
      for(
        ConcreteHL.FeedExpr( filter, cnxns ) <- feedExprStrm.take( numFeedExprs )
      ) {
        agentMgr().feed( erql, erspl )( filter, cnxns )
      }
    }
    def doSomeScores(
      scoreExprStrm : Stream[ConcreteHL.ScoreExpr] = mkScoreExprStream(),
      maxScores : Int = 1000,
      minScores : Int = 1,
      rndm : scala.util.Random = new scala.util.Random()
    ) : Unit = {
      val numScoreExprs =
        scala.math.min( rndm.nextInt( maxScores ) + 1, minScores )
      val sessionID = UUID.randomUUID
      val erql = agentMgr().erql( sessionID )
      val erspl = agentMgr().erspl( sessionID )
      for(
        ConcreteHL.ScoreExpr( filter, cnxns, staff ) <- scoreExprStrm.take( numScoreExprs )
      ) {
        agentMgr().score( erql, erspl )( filter, cnxns, staff )
      }
    }
  }
}
