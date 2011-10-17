package com.biosimilarity.seleKt.model.ill.vm.illvm;
import com.biosimilarity.seleKt.model.ill.vm.illvm.Absyn.*;
/** BNFC-Generated Composition Visitor
*/

public class ComposVisitor<A> implements
  com.biosimilarity.seleKt.model.ill.vm.illvm.Absyn.State.Visitor<com.biosimilarity.seleKt.model.ill.vm.illvm.Absyn.State,A>,
  com.biosimilarity.seleKt.model.ill.vm.illvm.Absyn.Stack.Visitor<com.biosimilarity.seleKt.model.ill.vm.illvm.Absyn.Stack,A>,
  com.biosimilarity.seleKt.model.ill.vm.illvm.Absyn.Dump.Visitor<com.biosimilarity.seleKt.model.ill.vm.illvm.Absyn.Dump,A>,
  com.biosimilarity.seleKt.model.ill.vm.illvm.Absyn.EnvOrVal.Visitor<com.biosimilarity.seleKt.model.ill.vm.illvm.Absyn.EnvOrVal,A>,
  com.biosimilarity.seleKt.model.ill.vm.illvm.Absyn.Frame.Visitor<com.biosimilarity.seleKt.model.ill.vm.illvm.Absyn.Frame,A>,
  com.biosimilarity.seleKt.model.ill.vm.illvm.Absyn.ILLCode.Visitor<com.biosimilarity.seleKt.model.ill.vm.illvm.Absyn.ILLCode,A>,
  com.biosimilarity.seleKt.model.ill.vm.illvm.Absyn.Instruction.Visitor<com.biosimilarity.seleKt.model.ill.vm.illvm.Absyn.Instruction,A>,
  com.biosimilarity.seleKt.model.ill.vm.illvm.Absyn.Value.Visitor<com.biosimilarity.seleKt.model.ill.vm.illvm.Absyn.Value,A>,
  com.biosimilarity.seleKt.model.ill.vm.illvm.Absyn.Env.Visitor<com.biosimilarity.seleKt.model.ill.vm.illvm.Absyn.Env,A>
{
/* State */
    public State visit(com.biosimilarity.seleKt.model.ill.vm.illvm.Absyn.MACHINE p, A arg)
    {
      Stack stack_ = p.stack_.accept(this, arg);
      Env env_ = p.env_.accept(this, arg);
      ILLCode illcode_ = p.illcode_.accept(this, arg);
      Dump dump_ = p.dump_.accept(this, arg);

      return new com.biosimilarity.seleKt.model.ill.vm.illvm.Absyn.MACHINE(stack_, env_, illcode_, dump_);
    }

/* Stack */
    public Stack visit(com.biosimilarity.seleKt.model.ill.vm.illvm.Absyn.MSTACK p, A arg)
    {
      ListEnvOrVal listenvorval_ = new ListEnvOrVal();
      for (EnvOrVal x : p.listenvorval_) {
        listenvorval_.add(x.accept(this,arg));
      }

      return new com.biosimilarity.seleKt.model.ill.vm.illvm.Absyn.MSTACK(listenvorval_);
    }

/* Dump */
    public Dump visit(com.biosimilarity.seleKt.model.ill.vm.illvm.Absyn.MDUMP p, A arg)
    {
      ListFrame listframe_ = new ListFrame();
      for (Frame x : p.listframe_) {
        listframe_.add(x.accept(this,arg));
      }

      return new com.biosimilarity.seleKt.model.ill.vm.illvm.Absyn.MDUMP(listframe_);
    }

/* EnvOrVal */
    public EnvOrVal visit(com.biosimilarity.seleKt.model.ill.vm.illvm.Absyn.STACKENV p, A arg)
    {
      Env env_ = p.env_.accept(this, arg);

      return new com.biosimilarity.seleKt.model.ill.vm.illvm.Absyn.STACKENV(env_);
    }
    public EnvOrVal visit(com.biosimilarity.seleKt.model.ill.vm.illvm.Absyn.STACKVAL p, A arg)
    {
      Value value_ = p.value_.accept(this, arg);

      return new com.biosimilarity.seleKt.model.ill.vm.illvm.Absyn.STACKVAL(value_);
    }

/* Frame */
    public Frame visit(com.biosimilarity.seleKt.model.ill.vm.illvm.Absyn.STACKFRAME p, A arg)
    {
      Stack stack_ = p.stack_.accept(this, arg);
      Env env_ = p.env_.accept(this, arg);
      ILLCode illcode_ = p.illcode_.accept(this, arg);

      return new com.biosimilarity.seleKt.model.ill.vm.illvm.Absyn.STACKFRAME(stack_, env_, illcode_);
    }

/* ILLCode */
    public ILLCode visit(com.biosimilarity.seleKt.model.ill.vm.illvm.Absyn.CODESEQ p, A arg)
    {
      ListInstruction listinstruction_ = new ListInstruction();
      for (Instruction x : p.listinstruction_) {
        listinstruction_.add(x.accept(this,arg));
      }

      return new com.biosimilarity.seleKt.model.ill.vm.illvm.Absyn.CODESEQ(listinstruction_);
    }

/* Instruction */
    public Instruction visit(com.biosimilarity.seleKt.model.ill.vm.illvm.Absyn.PUSHENV p, A arg)
    {
      String illpushenv_ = p.illpushenv_;

      return new com.biosimilarity.seleKt.model.ill.vm.illvm.Absyn.PUSHENV(illpushenv_);
    }
    public Instruction visit(com.biosimilarity.seleKt.model.ill.vm.illvm.Absyn.HEAD p, A arg)
    {
      String illhd_ = p.illhd_;

      return new com.biosimilarity.seleKt.model.ill.vm.illvm.Absyn.HEAD(illhd_);
    }
    public Instruction visit(com.biosimilarity.seleKt.model.ill.vm.illvm.Absyn.TAIL p, A arg)
    {
      String illtl_ = p.illtl_;

      return new com.biosimilarity.seleKt.model.ill.vm.illvm.Absyn.TAIL(illtl_);
    }
    public Instruction visit(com.biosimilarity.seleKt.model.ill.vm.illvm.Absyn.RET p, A arg)
    {
      String illret_ = p.illret_;

      return new com.biosimilarity.seleKt.model.ill.vm.illvm.Absyn.RET(illret_);
    }
    public Instruction visit(com.biosimilarity.seleKt.model.ill.vm.illvm.Absyn.PUSH p, A arg)
    {
      String illpush_ = p.illpush_;

      return new com.biosimilarity.seleKt.model.ill.vm.illvm.Absyn.PUSH(illpush_);
    }
    public Instruction visit(com.biosimilarity.seleKt.model.ill.vm.illvm.Absyn.POP p, A arg)
    {
      String illpop_ = p.illpop_;

      return new com.biosimilarity.seleKt.model.ill.vm.illvm.Absyn.POP(illpop_);
    }
    public Instruction visit(com.biosimilarity.seleKt.model.ill.vm.illvm.Absyn.MAKEFCL p, A arg)
    {
      ILLCode illcode_ = p.illcode_.accept(this, arg);

      return new com.biosimilarity.seleKt.model.ill.vm.illvm.Absyn.MAKEFCL(illcode_);
    }
    public Instruction visit(com.biosimilarity.seleKt.model.ill.vm.illvm.Absyn.AP p, A arg)
    {
      String illap_ = p.illap_;

      return new com.biosimilarity.seleKt.model.ill.vm.illvm.Absyn.AP(illap_);
    }
    public Instruction visit(com.biosimilarity.seleKt.model.ill.vm.illvm.Absyn.UNIT p, A arg)
    {
      String illunit_ = p.illunit_;

      return new com.biosimilarity.seleKt.model.ill.vm.illvm.Absyn.UNIT(illunit_);
    }
    public Instruction visit(com.biosimilarity.seleKt.model.ill.vm.illvm.Absyn.UNUNIT p, A arg)
    {
      String illununit_ = p.illununit_;

      return new com.biosimilarity.seleKt.model.ill.vm.illvm.Absyn.UNUNIT(illununit_);
    }
    public Instruction visit(com.biosimilarity.seleKt.model.ill.vm.illvm.Absyn.PAIR p, A arg)
    {
      String illpair_ = p.illpair_;

      return new com.biosimilarity.seleKt.model.ill.vm.illvm.Absyn.PAIR(illpair_);
    }
    public Instruction visit(com.biosimilarity.seleKt.model.ill.vm.illvm.Absyn.UNPAIR p, A arg)
    {
      String illunpair_ = p.illunpair_;

      return new com.biosimilarity.seleKt.model.ill.vm.illvm.Absyn.UNPAIR(illunpair_);
    }
    public Instruction visit(com.biosimilarity.seleKt.model.ill.vm.illvm.Absyn.MAKECCL p, A arg)
    {
      ILLCode illcode_1 = p.illcode_1.accept(this, arg);
      ILLCode illcode_2 = p.illcode_2.accept(this, arg);

      return new com.biosimilarity.seleKt.model.ill.vm.illvm.Absyn.MAKECCL(illcode_1, illcode_2);
    }
    public Instruction visit(com.biosimilarity.seleKt.model.ill.vm.illvm.Absyn.FST p, A arg)
    {
      String illfst_ = p.illfst_;

      return new com.biosimilarity.seleKt.model.ill.vm.illvm.Absyn.FST(illfst_);
    }
    public Instruction visit(com.biosimilarity.seleKt.model.ill.vm.illvm.Absyn.SND p, A arg)
    {
      String illsnd_ = p.illsnd_;

      return new com.biosimilarity.seleKt.model.ill.vm.illvm.Absyn.SND(illsnd_);
    }
    public Instruction visit(com.biosimilarity.seleKt.model.ill.vm.illvm.Absyn.INL p, A arg)
    {
      String illinl_ = p.illinl_;

      return new com.biosimilarity.seleKt.model.ill.vm.illvm.Absyn.INL(illinl_);
    }
    public Instruction visit(com.biosimilarity.seleKt.model.ill.vm.illvm.Absyn.INR p, A arg)
    {
      String illinr_ = p.illinr_;

      return new com.biosimilarity.seleKt.model.ill.vm.illvm.Absyn.INR(illinr_);
    }
    public Instruction visit(com.biosimilarity.seleKt.model.ill.vm.illvm.Absyn.CASE p, A arg)
    {
      ILLCode illcode_1 = p.illcode_1.accept(this, arg);
      ILLCode illcode_2 = p.illcode_2.accept(this, arg);

      return new com.biosimilarity.seleKt.model.ill.vm.illvm.Absyn.CASE(illcode_1, illcode_2);
    }
    public Instruction visit(com.biosimilarity.seleKt.model.ill.vm.illvm.Absyn.MAKEOCL p, A arg)
    {
      ILLCode illcode_ = p.illcode_.accept(this, arg);

      return new com.biosimilarity.seleKt.model.ill.vm.illvm.Absyn.MAKEOCL(illcode_);
    }
    public Instruction visit(com.biosimilarity.seleKt.model.ill.vm.illvm.Absyn.READ p, A arg)
    {
      String illread_ = p.illread_;

      return new com.biosimilarity.seleKt.model.ill.vm.illvm.Absyn.READ(illread_);
    }
    public Instruction visit(com.biosimilarity.seleKt.model.ill.vm.illvm.Absyn.DUP p, A arg)
    {
      String illdup_ = p.illdup_;

      return new com.biosimilarity.seleKt.model.ill.vm.illvm.Absyn.DUP(illdup_);
    }

/* Value */
    public Value visit(com.biosimilarity.seleKt.model.ill.vm.illvm.Absyn.UnitV p, A arg)
    {

      return new com.biosimilarity.seleKt.model.ill.vm.illvm.Absyn.UnitV();
    }
    public Value visit(com.biosimilarity.seleKt.model.ill.vm.illvm.Absyn.PairV p, A arg)
    {
      Value value_1 = p.value_1.accept(this, arg);
      Value value_2 = p.value_2.accept(this, arg);

      return new com.biosimilarity.seleKt.model.ill.vm.illvm.Absyn.PairV(value_1, value_2);
    }
    public Value visit(com.biosimilarity.seleKt.model.ill.vm.illvm.Absyn.InlV p, A arg)
    {
      Value value_ = p.value_.accept(this, arg);

      return new com.biosimilarity.seleKt.model.ill.vm.illvm.Absyn.InlV(value_);
    }
    public Value visit(com.biosimilarity.seleKt.model.ill.vm.illvm.Absyn.InrV p, A arg)
    {
      Value value_ = p.value_.accept(this, arg);

      return new com.biosimilarity.seleKt.model.ill.vm.illvm.Absyn.InrV(value_);
    }
    public Value visit(com.biosimilarity.seleKt.model.ill.vm.illvm.Absyn.FclV p, A arg)
    {
      ILLCode illcode_ = p.illcode_.accept(this, arg);
      Env env_ = p.env_.accept(this, arg);

      return new com.biosimilarity.seleKt.model.ill.vm.illvm.Absyn.FclV(illcode_, env_);
    }
    public Value visit(com.biosimilarity.seleKt.model.ill.vm.illvm.Absyn.CclV p, A arg)
    {
      ILLCode illcode_1 = p.illcode_1.accept(this, arg);
      ILLCode illcode_2 = p.illcode_2.accept(this, arg);
      Env env_ = p.env_.accept(this, arg);

      return new com.biosimilarity.seleKt.model.ill.vm.illvm.Absyn.CclV(illcode_1, illcode_2, env_);
    }
    public Value visit(com.biosimilarity.seleKt.model.ill.vm.illvm.Absyn.OclV p, A arg)
    {
      ILLCode illcode_ = p.illcode_.accept(this, arg);
      Env env_ = p.env_.accept(this, arg);

      return new com.biosimilarity.seleKt.model.ill.vm.illvm.Absyn.OclV(illcode_, env_);
    }

/* Env */
    public Env visit(com.biosimilarity.seleKt.model.ill.vm.illvm.Absyn.ENVIRONMENT p, A arg)
    {
      ListValue listvalue_ = new ListValue();
      for (Value x : p.listvalue_) {
        listvalue_.add(x.accept(this,arg));
      }

      return new com.biosimilarity.seleKt.model.ill.vm.illvm.Absyn.ENVIRONMENT(listvalue_);
    }

}