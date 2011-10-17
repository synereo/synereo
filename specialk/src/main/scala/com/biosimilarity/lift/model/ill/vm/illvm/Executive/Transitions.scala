// -*- mode: Scala;-*- 
// Filename:    Transitions.scala 
// Authors:     lgm                                                    
// Creation:    Tue Aug  9 20:35:07 2011 
// Copyright:   Not supplied 
// Description: 
// ------------------------------------------------------------------------

package com.biosimilarity.seleKt.model.ill.vm.illvm.executive

import com.biosimilarity.seleKt.model.ill.vm.illvm.Absyn._

trait Transitions[Ctxt] {
  import SyntaxConversion._

  def push(
    env : List[Value],
    stack : List[Either[Env,Value]]
  ) : List[Either[Env,Value]] = {
    Left[Env,Value]( asEnv( env ) ) :: stack 
  }
  def push(
    v : Value,
    stack : List[Either[Env,Value]]
  ) : List[Either[Env,Value]] = {
    Right[Env,Value]( v ) :: stack 
  }

  def reduceOnce( machine : MACHINE, ctxt : Ctxt ) : ( MACHINE, Ctxt ) = {
    val ( tmstate, nctxt ) =     
      reduceOnce( asTMState( machine ), ctxt )
    ( asMachine( tmstate ), nctxt )
  }

  def reduceOnce( tmstate : TMState, ctxt : Ctxt ) : ( TMState, Ctxt ) = {
    tmstate.code match {
      case instr :: instructions => {
	instr match {
	  case pushEnv : PUSHENV => {
	    (
	      new TMState(
		push( tmstate.env, tmstate.stack ),
		tmstate.env,
		instructions,
		tmstate.dump
	      ),
	      ctxt
	    )
	  }
	  case hd : HEAD => {
	    tmstate.stack match {
	      case Left( stkEnv ) :: stkRest => {
		fromEnv( stkEnv ) match {
		  case v :: l => {
		    (
		      new TMState(
			push( v, tmstate.stack ),
			tmstate.env,
			instructions,
			tmstate.dump
		      ),
		      ctxt
		    )
		  }
		  case _ => {
		    throw new Exception(
		      (
			"machine execution error:"
			+ " attempting to execute HD with"
			+ " invalid environment form"
		      )
		    )
		  }
		}
	      }
	      case _ => {
		throw new Exception(
		  (
		    "machine execution error:"
		    + " attempting to execute HD with"
		    + " invalid stack form" 
		  )
		)
	      }
	    }
	  }
	  case tl : TAIL => {
	    tmstate.stack match {
	      case Left( stkEnv ) :: stkRest => {
		fromEnv( stkEnv ) match {
		  case v :: l => {
		    (
		      new TMState(
			push( l, tmstate.stack ),
			tmstate.env,
			instructions,
			tmstate.dump
		      ),
		      ctxt
		    )
		  }
		  case _ => {
		    throw new Exception(
		      (
			"machine execution error:"
			+ " attempting to execute TL with"
			+ " invalid environment form"
		      )
		    )
		  }
		}
	      }
	      case _ => {
		throw new Exception(
		  (
		    "machine execution error:"
		    + " attempting to execute TL with"
		    + " invalid stack form" 
		  )
		)
	      }
	    }
	  }
	  case ret : RET => {
	    tmstate.stack match {
	      case Right( v ) :: stkRest => {
		tmstate.dump match {
		  case frame :: frameRest => {
		    fromFrame( frame ) match {
		      case ( s, e, c ) => {
			(
			  new TMState(
			    push( v, s ),
			    e,
			    c,
			    frameRest
			  ),
			  ctxt
			)
		      }
		      case _ => {
			throw new Exception(
			  (
			    "machine execution error:"
			    + " attempting to execute RET with"
			    + " invalid stack form on dump"
			  )
			)
		      }
		    }
		  }
		  case _ => {
		    throw new Exception(
		      (
			"machine execution error:"
			+ " attempting to execute RET with"
			+ " invalid stack form on dump"
		      )
		    )
		  }
		}
	      }
	      case _ => {
		throw new Exception(
		  (
		    "machine execution error:"
		    + " attempting to execute RET with"
		    + " invalid stack form" 
		  )
		)
	      }
	    }
	  }
	  case push : PUSH => {
	    tmstate.stack match {
	      case Right( v ) :: stkRest => {
		(
		  new TMState(
		    stkRest,
		    v :: tmstate.env,
		    instructions,
		    tmstate.dump
		  ),
		  ctxt
		)
	      }
	      case _ => {
		throw new Exception(
		  (
		    "machine execution error:"
		    + " attempting to execute PUSH with"
		    + " invalid stack form" 
		  )
		)
	      }
	    }
	  }
	  case pop : POP => {
	    tmstate.env match {
	      case v :: envRest => {
		(
		  new TMState(
		    tmstate.stack,
		    envRest,
		    instructions,
		    tmstate.dump
		  ),
		  ctxt
		)
	      }
	      case _ => {
		throw new Exception(
		  (
		    "machine execution error:"
		    + " attempting to execute POP with"
		    + " invalid stack form" 
		  )
		)
	      }
	    }
	  }
	  case makefcl : MAKEFCL => {
	    val fcl = 
	      Right[Env,Value]( new FclV( makefcl.illcode_, tmstate.env ) )
	    (
	      new TMState(
		fcl :: tmstate.stack,
		tmstate.env,
		instructions,
		tmstate.dump
	      ),
	      ctxt
	    )
	  }
	  case unit : UNIT => {
	    (
	      new TMState(
		Right[Env,Value]( new UnitV() ) :: tmstate.stack,
		tmstate.env,
		instructions,
		tmstate.dump
	      ),
	      ctxt
	    )
	  }
	  case ununit : UNUNIT => {
	    tmstate.stack match {
	      case Right( v : UnitV ) :: stkRest => {
		(
		  new TMState(
		    stkRest,
		    tmstate.env,
		    instructions,
		    tmstate.dump
		  ),
		  ctxt
		)
	      }
	      case _ => {
		throw new Exception(
		  (
		    "machine execution error:"
		    + " attempting to execute UNUNIT with"
		    + " invalid stack form" 
		  )
		)
	      }
	    }
	  }
	  case pair : PAIR => {
	    tmstate.stack match {
	      case Right( v ) :: Right( w ) :: stkRest => {
		(
		  new TMState(
		    Right( new PairV( v, w ) ) :: stkRest,
		    tmstate.env,
		    instructions,
		    tmstate.dump
		  ),
		  ctxt
		)
	      }
	      case _ => {
		throw new Exception(
		  (
		    "machine execution error:"
		    + " attempting to execute PAIR with"
		    + " invalid stack form" 
		  )
		)
	      }
	    }
	  }
	  case unpair : UNPAIR => {
	    tmstate.stack match {
	      case Right( vw : PairV ) :: stkRest => {
		( 
		  new TMState(
		    Right( vw.value_1 ) :: Right( vw.value_2 ) :: stkRest,
		    tmstate.env,
		    instructions,
		    tmstate.dump
		  ),
		  ctxt
		)
	      }
	      case _ => {
		throw new Exception(
		  (
		    "machine execution error:"
		    + " attempting to execute UNPAIR with"
		    + " invalid stack form" 
		  )
		)
	      }
	    }
	  }
	  case ap : AP => {
	    tmstate.stack match {
	      case Right( fcl : FclV ) :: stkRest => {
		tmstate.env match {
		  case v :: envRest => {
		    val nframe = 
		      asFrame(
			tmstate.stack, 
			tmstate.env,
			instructions
		      )
		    (
		      new TMState(
			List(),
			v :: fromEnv( fcl.env_ ),
			fromILLCode( fcl.illcode_ ),
			nframe :: tmstate.dump
		      ),
		      ctxt
		    )
		  }
		  case _ => {
		    throw new Exception(
		      (
			"machine execution error:"
			+ " attempting to execute AP with"
			+ " invalid env form" 
		      )
		    )
		  }
		}		
	      }
	      case _ => {
		throw new Exception(
		  (
		    "machine execution error:"
		    + " attempting to execute AP with"
		    + " invalid stack form" 
		  )
		)
	      }
	    }
	  }
	  case makeccl : MAKECCL => {
	    val ccl = 
	      new CclV(
		asILLCode( makeccl.illcode_1 ),
		asILLCode( makeccl.illcode_2 ),
		asEnv( tmstate.env )
	      )
	    (
	      new TMState(
		Right[Env,Value]( ccl ) :: tmstate.stack,
		tmstate.env,
		instructions,
		tmstate.dump
	      ),
	      ctxt
	    )
	  }
	  case fst : FST => {
	    tmstate.stack match {	      
	      case Right( ccl : CclV ) :: stkRest => {
		val nframe = 
		  asFrame(
		    tmstate.stack, 
		    tmstate.env,
		    instructions
		  )
		( 
		  new TMState(
		    List(),
		    fromEnv( ccl.env_ ),
		    fromILLCode( ccl.illcode_1 ),
		    nframe :: tmstate.dump
		  ),
		  ctxt
		)
	      }
	      case _ => {
		throw new Exception(
		  (
		    "machine execution error:"
		    + " attempting to execute FST with"
		    + " invalid stack form" 
		  )
		)
	      }
	    }
	  }
	  case snd : SND => {
	    tmstate.stack match {	      
	      case Right( ccl : CclV ) :: stkRest => {
		val nframe = 
		  asFrame(
		    tmstate.stack, 
		    tmstate.env,
		    instructions
		  )
		( 
		  new TMState(
		    List(),
		    fromEnv( ccl.env_ ),
		    fromILLCode( ccl.illcode_2 ),
		    nframe :: tmstate.dump
		  ),
		  ctxt
		)
	      }
	      case _ => {
		throw new Exception(
		  (
		    "machine execution error:"
		    + " attempting to execute SND with"
		    + " invalid stack form" 
		  )
		)
	      }
	    }
	  }
	  case inl : INL => {
	    tmstate.stack match {
	      case Right( v ) :: stkRest => {
		(
		  new TMState(
		    Right[Env,Value]( new InlV( v ) ) :: stkRest,
		    tmstate.env,
		    instructions,
		    tmstate.dump
		  ),
		  ctxt
		)
	      }
	      case _ => {
		throw new Exception(
		  (
		    "machine execution error:"
		    + " attempting to execute INL with"
		    + " invalid stack form" 
		  )
		)
	      }
	    }
	  }
	  case inr : INR => {
	    tmstate.stack match {
	      case Right( v ) :: stkRest => {
		(
		  new TMState(
		    Right[Env,Value]( new InrV( v ) ) :: stkRest,
		    tmstate.env,
		    instructions,
		    tmstate.dump
		  ),
		  ctxt
		)
	      }
	      case _ => {
		throw new Exception(
		  (
		    "machine execution error:"
		    + " attempting to execute INR with"
		    + " invalid stack form" 
		  )
		)
	      }
	    }
	  }
	  case kase : CASE => {
	    tmstate.stack match {
	      case Right( v : InlV ) :: stkRest => {
		val nframe = 
		  asFrame(
		    tmstate.stack, 
		    tmstate.env,
		    instructions
		  )
		(
		  new TMState(
		    List(),
		    v.value_ :: tmstate.env,
		    fromILLCode( kase.illcode_1 ),
		    nframe :: tmstate.dump
		  ),
		  ctxt
		)
	      }
	      case Right( v : InrV ) :: stkRest => {
		val nframe = 
		  asFrame(
		    tmstate.stack, 
		    tmstate.env,
		    instructions
		  )
		(
		  new TMState(
		    List(),
		    v.value_ :: tmstate.env,
		    fromILLCode( kase.illcode_2 ),
		    nframe :: tmstate.dump
		  ),
		  ctxt
		)
	      }
	      case _ => {
		throw new Exception(
		  (
		    "machine execution error:"
		    + " attempting to execute CASE with"
		    + " invalid stack form" 
		  )
		)
	      }
	    }
	  }
	  case makeocl : MAKEOCL => {
	    val ocl = 
	      Right[Env,Value]( new OclV( makeocl.illcode_, tmstate.env ) )
	    (
	      new TMState(
		ocl :: tmstate.stack,
		tmstate.env,
		instructions,
		tmstate.dump
	      ),
	      ctxt
	    )
	  }
	  case rd : READ => {
	    tmstate.stack match {	      
	      case Right( ocl : OclV ) :: stkRest => {
		val nframe = 
		  asFrame(
		    tmstate.stack, 
		    tmstate.env,
		    instructions
		  )
		( 
		  new TMState(
		    List(),
		    fromEnv( ocl.env_ ),
		    fromILLCode( ocl.illcode_ ),
		    nframe :: tmstate.dump
		  ),
		  ctxt
		)
	      }
	      case _ => {
		throw new Exception(
		  (
		    "machine execution error:"
		    + " attempting to execute SND with"
		    + " invalid stack form" 
		  )
		)
	      }
	    }
	  }
	  case dup : DUP => {
	    tmstate.stack match {
	      case Right( v ) :: stkRest => {
		(
		  new TMState(
		    Right( v ) :: Right( v ) :: stkRest,
		    tmstate.env,
		    instructions,
		    tmstate.dump
		  ),
		  ctxt
		)
	      }
	      case _ => {
		throw new Exception(
		  (
		    "machine execution error:"
		    + " attempting to execute DUP with"
		    + " invalid stack form" 
		  )
		)
	      }
	    }
	  }
	}
      }
      case _ => ( tmstate, ctxt )
    }
    
  }

  def reduce( machine : MACHINE, ctxt : Ctxt ) : ( Value, Ctxt ) = {
    reduce( asTMState( machine ), ctxt )
  }

  def reduce( tmstate : TMState, ctxt : Ctxt ) : ( Value, Ctxt ) = {
    var vmstate = tmstate
    var ktxt = ctxt 

    while( !( vmstate.code.isEmpty ) ) {
      reduceOnce( vmstate, ctxt ) match {
	case ( r1state, nCtxt ) => {
	  vmstate = r1state
	  ktxt = nCtxt
	}
      }
    }

    vmstate.stack match {
      case Right( v ) :: stkRest => ( v, ktxt )
      case _ => throw new Exception( "execution failed to produce a value" )
    }
  }
}

object NoCtxtTransitions extends Transitions[Unit] {
}

object TraceTransitions extends Transitions[ReadBack] {
  import SyntaxConversion._
  override def reduce(
    tmstate : TMState,
    ctxt : ReadBack
  ) : ( Value, ReadBack ) = {
    var vmstate = tmstate
    var ktxt = ctxt 

    while( !( vmstate.code.isEmpty ) ) {
      println( ctxt.prettyPrint( vmstate )( 0 ) )
      println( " ==========> " )
      reduceOnce( vmstate, ctxt ) match {
	case ( r1state, nCtxt ) => {
	  vmstate = r1state
	  ktxt = nCtxt
	}
      }
    }

    vmstate.stack match {
      case Right( v ) :: stkRest => ( v, ktxt )
      case _ => throw new Exception( "execution failed to produce a value" )
    }
  }
}
