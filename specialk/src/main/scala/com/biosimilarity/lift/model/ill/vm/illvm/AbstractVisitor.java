package com.biosimilarity.seleKt.model.ill.vm.illvm;
import com.biosimilarity.seleKt.model.ill.vm.illvm.Absyn.*;
/** BNFC-Generated Abstract Visitor */
public class AbstractVisitor<R,A> implements AllVisitor<R,A> {
/* State */
    public R visit(com.biosimilarity.seleKt.model.ill.vm.illvm.Absyn.MACHINE p, A arg) { return visitDefault(p, arg); }
    public R visitDefault(com.biosimilarity.seleKt.model.ill.vm.illvm.Absyn.State p, A arg) {
      throw new IllegalArgumentException(this.getClass().getName() + ": " + p);
    }
/* Stack */
    public R visit(com.biosimilarity.seleKt.model.ill.vm.illvm.Absyn.MSTACK p, A arg) { return visitDefault(p, arg); }
    public R visitDefault(com.biosimilarity.seleKt.model.ill.vm.illvm.Absyn.Stack p, A arg) {
      throw new IllegalArgumentException(this.getClass().getName() + ": " + p);
    }
/* Dump */
    public R visit(com.biosimilarity.seleKt.model.ill.vm.illvm.Absyn.MDUMP p, A arg) { return visitDefault(p, arg); }
    public R visitDefault(com.biosimilarity.seleKt.model.ill.vm.illvm.Absyn.Dump p, A arg) {
      throw new IllegalArgumentException(this.getClass().getName() + ": " + p);
    }
/* EnvOrVal */
    public R visit(com.biosimilarity.seleKt.model.ill.vm.illvm.Absyn.STACKENV p, A arg) { return visitDefault(p, arg); }
    public R visit(com.biosimilarity.seleKt.model.ill.vm.illvm.Absyn.STACKVAL p, A arg) { return visitDefault(p, arg); }
    public R visitDefault(com.biosimilarity.seleKt.model.ill.vm.illvm.Absyn.EnvOrVal p, A arg) {
      throw new IllegalArgumentException(this.getClass().getName() + ": " + p);
    }
/* Frame */
    public R visit(com.biosimilarity.seleKt.model.ill.vm.illvm.Absyn.STACKFRAME p, A arg) { return visitDefault(p, arg); }
    public R visitDefault(com.biosimilarity.seleKt.model.ill.vm.illvm.Absyn.Frame p, A arg) {
      throw new IllegalArgumentException(this.getClass().getName() + ": " + p);
    }
/* ILLCode */
    public R visit(com.biosimilarity.seleKt.model.ill.vm.illvm.Absyn.CODESEQ p, A arg) { return visitDefault(p, arg); }
    public R visitDefault(com.biosimilarity.seleKt.model.ill.vm.illvm.Absyn.ILLCode p, A arg) {
      throw new IllegalArgumentException(this.getClass().getName() + ": " + p);
    }
/* Instruction */
    public R visit(com.biosimilarity.seleKt.model.ill.vm.illvm.Absyn.PUSHENV p, A arg) { return visitDefault(p, arg); }
    public R visit(com.biosimilarity.seleKt.model.ill.vm.illvm.Absyn.HEAD p, A arg) { return visitDefault(p, arg); }
    public R visit(com.biosimilarity.seleKt.model.ill.vm.illvm.Absyn.TAIL p, A arg) { return visitDefault(p, arg); }
    public R visit(com.biosimilarity.seleKt.model.ill.vm.illvm.Absyn.RET p, A arg) { return visitDefault(p, arg); }
    public R visit(com.biosimilarity.seleKt.model.ill.vm.illvm.Absyn.PUSH p, A arg) { return visitDefault(p, arg); }
    public R visit(com.biosimilarity.seleKt.model.ill.vm.illvm.Absyn.POP p, A arg) { return visitDefault(p, arg); }
    public R visit(com.biosimilarity.seleKt.model.ill.vm.illvm.Absyn.MAKEFCL p, A arg) { return visitDefault(p, arg); }
    public R visit(com.biosimilarity.seleKt.model.ill.vm.illvm.Absyn.AP p, A arg) { return visitDefault(p, arg); }
    public R visit(com.biosimilarity.seleKt.model.ill.vm.illvm.Absyn.UNIT p, A arg) { return visitDefault(p, arg); }
    public R visit(com.biosimilarity.seleKt.model.ill.vm.illvm.Absyn.UNUNIT p, A arg) { return visitDefault(p, arg); }
    public R visit(com.biosimilarity.seleKt.model.ill.vm.illvm.Absyn.PAIR p, A arg) { return visitDefault(p, arg); }
    public R visit(com.biosimilarity.seleKt.model.ill.vm.illvm.Absyn.UNPAIR p, A arg) { return visitDefault(p, arg); }
    public R visit(com.biosimilarity.seleKt.model.ill.vm.illvm.Absyn.MAKECCL p, A arg) { return visitDefault(p, arg); }
    public R visit(com.biosimilarity.seleKt.model.ill.vm.illvm.Absyn.FST p, A arg) { return visitDefault(p, arg); }
    public R visit(com.biosimilarity.seleKt.model.ill.vm.illvm.Absyn.SND p, A arg) { return visitDefault(p, arg); }
    public R visit(com.biosimilarity.seleKt.model.ill.vm.illvm.Absyn.INL p, A arg) { return visitDefault(p, arg); }
    public R visit(com.biosimilarity.seleKt.model.ill.vm.illvm.Absyn.INR p, A arg) { return visitDefault(p, arg); }
    public R visit(com.biosimilarity.seleKt.model.ill.vm.illvm.Absyn.CASE p, A arg) { return visitDefault(p, arg); }
    public R visit(com.biosimilarity.seleKt.model.ill.vm.illvm.Absyn.MAKEOCL p, A arg) { return visitDefault(p, arg); }
    public R visit(com.biosimilarity.seleKt.model.ill.vm.illvm.Absyn.READ p, A arg) { return visitDefault(p, arg); }
    public R visit(com.biosimilarity.seleKt.model.ill.vm.illvm.Absyn.DUP p, A arg) { return visitDefault(p, arg); }
    public R visitDefault(com.biosimilarity.seleKt.model.ill.vm.illvm.Absyn.Instruction p, A arg) {
      throw new IllegalArgumentException(this.getClass().getName() + ": " + p);
    }
/* Value */
    public R visit(com.biosimilarity.seleKt.model.ill.vm.illvm.Absyn.UnitV p, A arg) { return visitDefault(p, arg); }
    public R visit(com.biosimilarity.seleKt.model.ill.vm.illvm.Absyn.PairV p, A arg) { return visitDefault(p, arg); }
    public R visit(com.biosimilarity.seleKt.model.ill.vm.illvm.Absyn.InlV p, A arg) { return visitDefault(p, arg); }
    public R visit(com.biosimilarity.seleKt.model.ill.vm.illvm.Absyn.InrV p, A arg) { return visitDefault(p, arg); }
    public R visit(com.biosimilarity.seleKt.model.ill.vm.illvm.Absyn.FclV p, A arg) { return visitDefault(p, arg); }
    public R visit(com.biosimilarity.seleKt.model.ill.vm.illvm.Absyn.CclV p, A arg) { return visitDefault(p, arg); }
    public R visit(com.biosimilarity.seleKt.model.ill.vm.illvm.Absyn.OclV p, A arg) { return visitDefault(p, arg); }
    public R visitDefault(com.biosimilarity.seleKt.model.ill.vm.illvm.Absyn.Value p, A arg) {
      throw new IllegalArgumentException(this.getClass().getName() + ": " + p);
    }
/* Env */
    public R visit(com.biosimilarity.seleKt.model.ill.vm.illvm.Absyn.ENVIRONMENT p, A arg) { return visitDefault(p, arg); }
    public R visitDefault(com.biosimilarity.seleKt.model.ill.vm.illvm.Absyn.Env p, A arg) {
      throw new IllegalArgumentException(this.getClass().getName() + ": " + p);
    }

}
