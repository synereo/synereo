package com.biosimilarity.seleKt.model.ill.vm.illvm.Absyn; // Java Package generated by the BNF Converter.

public class PAIR extends Instruction {
  public final String illpair_;

  public PAIR(String p1) { illpair_ = p1; }

  public <R,A> R accept(com.biosimilarity.seleKt.model.ill.vm.illvm.Absyn.Instruction.Visitor<R,A> v, A arg) { return v.visit(this, arg); }

  public boolean equals(Object o) {
    if (this == o) return true;
    if (o instanceof com.biosimilarity.seleKt.model.ill.vm.illvm.Absyn.PAIR) {
      com.biosimilarity.seleKt.model.ill.vm.illvm.Absyn.PAIR x = (com.biosimilarity.seleKt.model.ill.vm.illvm.Absyn.PAIR)o;
      return this.illpair_.equals(x.illpair_);
    }
    return false;
  }

  public int hashCode() {
    return this.illpair_.hashCode();
  }


}