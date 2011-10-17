package com.biosimilarity.seleKt.model.ill.vm.illvm;

import com.biosimilarity.seleKt.model.ill.vm.illvm.Absyn.*;

/** BNFC-Generated All Visitor */
public interface AllVisitor<R,A> extends
  com.biosimilarity.seleKt.model.ill.vm.illvm.Absyn.State.Visitor<R,A>,
  com.biosimilarity.seleKt.model.ill.vm.illvm.Absyn.Stack.Visitor<R,A>,
  com.biosimilarity.seleKt.model.ill.vm.illvm.Absyn.Dump.Visitor<R,A>,
  com.biosimilarity.seleKt.model.ill.vm.illvm.Absyn.EnvOrVal.Visitor<R,A>,
  com.biosimilarity.seleKt.model.ill.vm.illvm.Absyn.Frame.Visitor<R,A>,
  com.biosimilarity.seleKt.model.ill.vm.illvm.Absyn.ILLCode.Visitor<R,A>,
  com.biosimilarity.seleKt.model.ill.vm.illvm.Absyn.Instruction.Visitor<R,A>,
  com.biosimilarity.seleKt.model.ill.vm.illvm.Absyn.Value.Visitor<R,A>,
  com.biosimilarity.seleKt.model.ill.vm.illvm.Absyn.Env.Visitor<R,A>
{}
