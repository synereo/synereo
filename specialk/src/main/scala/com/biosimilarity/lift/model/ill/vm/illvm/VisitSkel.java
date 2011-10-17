package com.biosimilarity.seleKt.model.ill.vm.illvm;
import com.biosimilarity.seleKt.model.ill.vm.illvm.Absyn.*;
/*** BNFC-Generated Visitor Design Pattern Skeleton. ***/
/* This implements the common visitor design pattern.
   Tests show it to be slightly less efficient than the
   instanceof method, but easier to use. 
   Replace the R and A parameters with the desired return
   and context types.*/

public class VisitSkel
{
  public class StateVisitor<R,A> implements State.Visitor<R,A>
  {
    public R visit(com.biosimilarity.seleKt.model.ill.vm.illvm.Absyn.MACHINE p, A arg)
    {
      /* Code For MACHINE Goes Here */

      p.stack_.accept(new StackVisitor<R,A>(), arg);
      p.env_.accept(new EnvVisitor<R,A>(), arg);
      p.illcode_.accept(new ILLCodeVisitor<R,A>(), arg);
      p.dump_.accept(new DumpVisitor<R,A>(), arg);

      return null;
    }

  }
  public class StackVisitor<R,A> implements Stack.Visitor<R,A>
  {
    public R visit(com.biosimilarity.seleKt.model.ill.vm.illvm.Absyn.MSTACK p, A arg)
    {
      /* Code For MSTACK Goes Here */

      for (EnvOrVal x : p.listenvorval_) {
      }

      return null;
    }

  }
  public class DumpVisitor<R,A> implements Dump.Visitor<R,A>
  {
    public R visit(com.biosimilarity.seleKt.model.ill.vm.illvm.Absyn.MDUMP p, A arg)
    {
      /* Code For MDUMP Goes Here */

      for (Frame x : p.listframe_) {
      }

      return null;
    }

  }
  public class EnvOrValVisitor<R,A> implements EnvOrVal.Visitor<R,A>
  {
    public R visit(com.biosimilarity.seleKt.model.ill.vm.illvm.Absyn.STACKENV p, A arg)
    {
      /* Code For STACKENV Goes Here */

      p.env_.accept(new EnvVisitor<R,A>(), arg);

      return null;
    }
    public R visit(com.biosimilarity.seleKt.model.ill.vm.illvm.Absyn.STACKVAL p, A arg)
    {
      /* Code For STACKVAL Goes Here */

      p.value_.accept(new ValueVisitor<R,A>(), arg);

      return null;
    }

  }
  public class FrameVisitor<R,A> implements Frame.Visitor<R,A>
  {
    public R visit(com.biosimilarity.seleKt.model.ill.vm.illvm.Absyn.STACKFRAME p, A arg)
    {
      /* Code For STACKFRAME Goes Here */

      p.stack_.accept(new StackVisitor<R,A>(), arg);
      p.env_.accept(new EnvVisitor<R,A>(), arg);
      p.illcode_.accept(new ILLCodeVisitor<R,A>(), arg);

      return null;
    }

  }
  public class ILLCodeVisitor<R,A> implements ILLCode.Visitor<R,A>
  {
    public R visit(com.biosimilarity.seleKt.model.ill.vm.illvm.Absyn.CODESEQ p, A arg)
    {
      /* Code For CODESEQ Goes Here */

      for (Instruction x : p.listinstruction_) {
      }

      return null;
    }

  }
  public class InstructionVisitor<R,A> implements Instruction.Visitor<R,A>
  {
    public R visit(com.biosimilarity.seleKt.model.ill.vm.illvm.Absyn.PUSHENV p, A arg)
    {
      /* Code For PUSHENV Goes Here */

      //p.illpushenv_;

      return null;
    }
    public R visit(com.biosimilarity.seleKt.model.ill.vm.illvm.Absyn.HEAD p, A arg)
    {
      /* Code For HEAD Goes Here */

      //p.illhd_;

      return null;
    }
    public R visit(com.biosimilarity.seleKt.model.ill.vm.illvm.Absyn.TAIL p, A arg)
    {
      /* Code For TAIL Goes Here */

      //p.illtl_;

      return null;
    }
    public R visit(com.biosimilarity.seleKt.model.ill.vm.illvm.Absyn.RET p, A arg)
    {
      /* Code For RET Goes Here */

      //p.illret_;

      return null;
    }
    public R visit(com.biosimilarity.seleKt.model.ill.vm.illvm.Absyn.PUSH p, A arg)
    {
      /* Code For PUSH Goes Here */

      //p.illpush_;

      return null;
    }
    public R visit(com.biosimilarity.seleKt.model.ill.vm.illvm.Absyn.POP p, A arg)
    {
      /* Code For POP Goes Here */

      //p.illpop_;

      return null;
    }
    public R visit(com.biosimilarity.seleKt.model.ill.vm.illvm.Absyn.MAKEFCL p, A arg)
    {
      /* Code For MAKEFCL Goes Here */

      p.illcode_.accept(new ILLCodeVisitor<R,A>(), arg);

      return null;
    }
    public R visit(com.biosimilarity.seleKt.model.ill.vm.illvm.Absyn.AP p, A arg)
    {
      /* Code For AP Goes Here */

      //p.illap_;

      return null;
    }
    public R visit(com.biosimilarity.seleKt.model.ill.vm.illvm.Absyn.UNIT p, A arg)
    {
      /* Code For UNIT Goes Here */

      //p.illunit_;

      return null;
    }
    public R visit(com.biosimilarity.seleKt.model.ill.vm.illvm.Absyn.UNUNIT p, A arg)
    {
      /* Code For UNUNIT Goes Here */

      //p.illununit_;

      return null;
    }
    public R visit(com.biosimilarity.seleKt.model.ill.vm.illvm.Absyn.PAIR p, A arg)
    {
      /* Code For PAIR Goes Here */

      //p.illpair_;

      return null;
    }
    public R visit(com.biosimilarity.seleKt.model.ill.vm.illvm.Absyn.UNPAIR p, A arg)
    {
      /* Code For UNPAIR Goes Here */

      //p.illunpair_;

      return null;
    }
    public R visit(com.biosimilarity.seleKt.model.ill.vm.illvm.Absyn.MAKECCL p, A arg)
    {
      /* Code For MAKECCL Goes Here */

      p.illcode_1.accept(new ILLCodeVisitor<R,A>(), arg);
      p.illcode_2.accept(new ILLCodeVisitor<R,A>(), arg);

      return null;
    }
    public R visit(com.biosimilarity.seleKt.model.ill.vm.illvm.Absyn.FST p, A arg)
    {
      /* Code For FST Goes Here */

      //p.illfst_;

      return null;
    }
    public R visit(com.biosimilarity.seleKt.model.ill.vm.illvm.Absyn.SND p, A arg)
    {
      /* Code For SND Goes Here */

      //p.illsnd_;

      return null;
    }
    public R visit(com.biosimilarity.seleKt.model.ill.vm.illvm.Absyn.INL p, A arg)
    {
      /* Code For INL Goes Here */

      //p.illinl_;

      return null;
    }
    public R visit(com.biosimilarity.seleKt.model.ill.vm.illvm.Absyn.INR p, A arg)
    {
      /* Code For INR Goes Here */

      //p.illinr_;

      return null;
    }
    public R visit(com.biosimilarity.seleKt.model.ill.vm.illvm.Absyn.CASE p, A arg)
    {
      /* Code For CASE Goes Here */

      p.illcode_1.accept(new ILLCodeVisitor<R,A>(), arg);
      p.illcode_2.accept(new ILLCodeVisitor<R,A>(), arg);

      return null;
    }
    public R visit(com.biosimilarity.seleKt.model.ill.vm.illvm.Absyn.MAKEOCL p, A arg)
    {
      /* Code For MAKEOCL Goes Here */

      p.illcode_.accept(new ILLCodeVisitor<R,A>(), arg);

      return null;
    }
    public R visit(com.biosimilarity.seleKt.model.ill.vm.illvm.Absyn.READ p, A arg)
    {
      /* Code For READ Goes Here */

      //p.illread_;

      return null;
    }
    public R visit(com.biosimilarity.seleKt.model.ill.vm.illvm.Absyn.DUP p, A arg)
    {
      /* Code For DUP Goes Here */

      //p.illdup_;

      return null;
    }

  }
  public class ValueVisitor<R,A> implements Value.Visitor<R,A>
  {
    public R visit(com.biosimilarity.seleKt.model.ill.vm.illvm.Absyn.UnitV p, A arg)
    {
      /* Code For UnitV Goes Here */


      return null;
    }
    public R visit(com.biosimilarity.seleKt.model.ill.vm.illvm.Absyn.PairV p, A arg)
    {
      /* Code For PairV Goes Here */

      p.value_1.accept(new ValueVisitor<R,A>(), arg);
      p.value_2.accept(new ValueVisitor<R,A>(), arg);

      return null;
    }
    public R visit(com.biosimilarity.seleKt.model.ill.vm.illvm.Absyn.InlV p, A arg)
    {
      /* Code For InlV Goes Here */

      p.value_.accept(new ValueVisitor<R,A>(), arg);

      return null;
    }
    public R visit(com.biosimilarity.seleKt.model.ill.vm.illvm.Absyn.InrV p, A arg)
    {
      /* Code For InrV Goes Here */

      p.value_.accept(new ValueVisitor<R,A>(), arg);

      return null;
    }
    public R visit(com.biosimilarity.seleKt.model.ill.vm.illvm.Absyn.FclV p, A arg)
    {
      /* Code For FclV Goes Here */

      p.illcode_.accept(new ILLCodeVisitor<R,A>(), arg);
      p.env_.accept(new EnvVisitor<R,A>(), arg);

      return null;
    }
    public R visit(com.biosimilarity.seleKt.model.ill.vm.illvm.Absyn.CclV p, A arg)
    {
      /* Code For CclV Goes Here */

      p.illcode_1.accept(new ILLCodeVisitor<R,A>(), arg);
      p.illcode_2.accept(new ILLCodeVisitor<R,A>(), arg);
      p.env_.accept(new EnvVisitor<R,A>(), arg);

      return null;
    }
    public R visit(com.biosimilarity.seleKt.model.ill.vm.illvm.Absyn.OclV p, A arg)
    {
      /* Code For OclV Goes Here */

      p.illcode_.accept(new ILLCodeVisitor<R,A>(), arg);
      p.env_.accept(new EnvVisitor<R,A>(), arg);

      return null;
    }

  }
  public class EnvVisitor<R,A> implements Env.Visitor<R,A>
  {
    public R visit(com.biosimilarity.seleKt.model.ill.vm.illvm.Absyn.ENVIRONMENT p, A arg)
    {
      /* Code For ENVIRONMENT Goes Here */

      for (Value x : p.listvalue_) {
      }

      return null;
    }

  }
}