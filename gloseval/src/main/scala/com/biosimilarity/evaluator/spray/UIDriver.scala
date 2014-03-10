// -*- mode: Scala;-*- 
// Filename:    UIDriver.scala 
// Authors:     lgm                                                    
// Creation:    Wed Mar  5 10:07:05 2014 
// Copyright:   Not supplied 
// Description: 
// ------------------------------------------------------------------------

package com.biosimilarity.evaluator.spray
import com.biosimilarity.evaluator.distribution._
import com.biosimilarity.evaluator.msgs._
import com.protegra_ati.agentservices.store.util._
import com.biosimilarity.lift.model.store._
import com.biosimilarity.lift.lib._


package usage {
  object CurlReqMkr {
    import com.biosimilarity.evaluator.spray.Serializer
    import com.biosimilarity.evaluator.distribution.PortableAgentCnxn
    import com.protegra_ati.agentservices.store.extensions.StringExtensions._
    import DSLCommLink.mTT
    import ConcreteHL._
    import com.biosimilarity.lift.model.store._
    import java.util.UUID

    def getAliasCnxns(
      emailAddr : String,
      aliasCnxnK : List[PortableAgentCnxn] => Unit,
      fail: String => Unit = println _
    ) : Unit = {
      val auid =
        EvalAndAgentCRUDHandlerService.emailToCap( emailAddr )
      val aliasCnxn =
        PortableAgentCnxn( auid.toURI, "alias", auid.toURI )

      val onFetch : Option[mTT.Resource] => Unit = ( optRsrc : Option[mTT.Resource] ) => {
        optRsrc match {
          case None => {
            // Nothing to be done
            BasicLogService.tweet( "getAliasCnxns | onFetch: got None" )
          }
          case Some( mTT.RBoundHM( Some( mTT.Ground( v ) ), _ ) ) => {
            BasicLogService.tweet( "getAliasCnxns | onFetch: got " + v )
            
            val biCnxnList = v match {
              case PostedExpr( (PostedExpr( biCnxnListStr : String ), _, _, _) ) => {
                Serializer.deserialize[List[PortableAgentBiCnxn]]( biCnxnListStr )
              }
              case Bottom => Nil
            }
            
            aliasCnxnK( biCnxnList.map( _.writeCnxn ) )
          }
          case _ => fail("getAliasCnxns -- Unrecognized resource: " + optRsrc)
        }
      }

      //EvalAndAgentCRUDHandlerService.agentMgr().fetch(
      EvalAndAgentCRUDHandlerService.fetch(
        EvalAndAgentCRUDHandlerService.biCnxnsStorageLocation,
        List( aliasCnxn ),
        onFetch
      )
    }

    def uuid2SessionURI( uuidStr : String ) : String = {
      "agent-session://" + uuidStr
    }
    def dbName2Cnxn( dbNameStr : String ) : PortableAgentCnxn = {
      val ( src, lbl, trgt ) = 
        (
          dbNameStr.substring( 0, 20 ),
          dbNameStr.substring( 20, 8 ),
          dbNameStr.substring( 28, 48 )
        );
      
      PortableAgentCnxn( src.toURI, lbl, trgt.toURI )
    }

    def runcURLReq(
      cURLReq : String,
      onProc : RunProcessResponse => Unit = { rsp => println( rsp ) }
    ) : Unit = {
      onProc(
        ProcessRunner.run(
          ConcreteHL.RunProcessRequest(
            cURLReq,
            None,
            Nil
          )
        )
      )
    }

    def mkNRunReq(
      client : String,
      emailAddr : String,
      correlationId : String = UUID.randomUUID.toString,
      clm : String = "claim( This )",
      onProc : RunProcessResponse => Unit = { reqStr => println( reqStr ) }
    ) : Unit = {
      mkReq(
        client,
        emailAddr,
        correlationId,
        clm,
        {
          ( req : String ) => {
            runcURLReq(
              req,
              { ( procRsp : RunProcessResponse ) => println( procRsp )
             }
            )
          }
        }
      )
    }

    def mkReq(
      client : String,
      emailAddr : String,
      correlationId : String = UUID.randomUUID.toString,
      clm : String = "claim( This )",
      onReqCtor : String => Unit = { reqStr => println( reqStr ) }
    ) : Unit = {
      val auid =
        
      getAliasCnxns(
        emailAddr,
        {
          ( cnxnsList : List[PortableAgentCnxn] ) => {
            val vrfr :: rp :: _ = cnxnsList
            val reqStr =
              mkReq( 
                client,
                EvalAndAgentCRUDHandlerService.emailToCap( emailAddr ).toURI.toString,
                correlationId,
                vrfr,
                rp, 
                clm.toLabel
              )
            onReqCtor( reqStr )
          }
        }
      )
    }

    def mkReq( 
      client : String,
      sessionURI : String,
      correlationId : String,
      vrfr : PortableAgentCnxn,
      rp : PortableAgentCnxn,
      clm : CnxnCtxtLabel[String,String,String]
    ) : String = {
      val cleanClm = clm.toString.replace( "'", "" )
      s"""curl http://${client}:9876/api -d '{"msgType":"initiateClaim","content": {"sessionURI":"${sessionURI}","correlationId":"${correlationId}","verifier": {"source":"${vrfr.src}","label":"claimantToSelf","target":"${vrfr.trgt}"},"relyingParty": {"source":"${rp.src}","label":"claimantToSelf","target":"${rp.trgt}"},"claim":"${cleanClm}"}}'"""
    }

    def mkReq(
      client : String,
      c2vDBName : String,
      c2RPDBName : String,    
      clm : String
    ) : String = {
      val correlationId = UUID.randomUUID().toString
      val ( vrfr, rp ) =
        (
          dbName2Cnxn( c2vDBName ),
          dbName2Cnxn( c2RPDBName )
        );
      val sessionURI = vrfr.src.toString
      mkReq(
        client,
        sessionURI,
        correlationId,
        vrfr,
        rp,
        clm.toLabel
      )
    }
  }
}

