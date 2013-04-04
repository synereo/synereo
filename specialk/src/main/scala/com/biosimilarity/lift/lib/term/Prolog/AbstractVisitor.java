package com.biosimilarity.lift.lib.term.Prolog;
import com.biosimilarity.lift.lib.term.Prolog.Absyn.*;
/** BNFC-Generated Abstract Visitor */
public class AbstractVisitor<R,A> implements AllVisitor<R,A> {
/* Predicate */
    public R visit(com.biosimilarity.lift.lib.term.Prolog.Absyn.APred p, A arg) { return visitDefault(p, arg); }
    public R visit(com.biosimilarity.lift.lib.term.Prolog.Absyn.CPred p, A arg) { return visitDefault(p, arg); }
    public R visitDefault(com.biosimilarity.lift.lib.term.Prolog.Absyn.Predicate p, A arg) {
      throw new IllegalArgumentException(this.getClass().getName() + ": " + p);
    }
/* Term */
    public R visit(com.biosimilarity.lift.lib.term.Prolog.Absyn.TAtom p, A arg) { return visitDefault(p, arg); }
    public R visit(com.biosimilarity.lift.lib.term.Prolog.Absyn.VarT p, A arg) { return visitDefault(p, arg); }
    public R visit(com.biosimilarity.lift.lib.term.Prolog.Absyn.Complex p, A arg) { return visitDefault(p, arg); }
    public R visit(com.biosimilarity.lift.lib.term.Prolog.Absyn.TList p, A arg) { return visitDefault(p, arg); }
    public R visitDefault(com.biosimilarity.lift.lib.term.Prolog.Absyn.Term p, A arg) {
      throw new IllegalArgumentException(this.getClass().getName() + ": " + p);
    }
/* Atom */
    public R visit(com.biosimilarity.lift.lib.term.Prolog.Absyn.Atm p, A arg) { return visitDefault(p, arg); }
    public R visit(com.biosimilarity.lift.lib.term.Prolog.Absyn.EAtm p, A arg) { return visitDefault(p, arg); }
    public R visit(com.biosimilarity.lift.lib.term.Prolog.Absyn.BAtm p, A arg) { return visitDefault(p, arg); }
    public R visit(com.biosimilarity.lift.lib.term.Prolog.Absyn.StrAtm p, A arg) { return visitDefault(p, arg); }
    public R visit(com.biosimilarity.lift.lib.term.Prolog.Absyn.IntAtm p, A arg) { return visitDefault(p, arg); }
    public R visit(com.biosimilarity.lift.lib.term.Prolog.Absyn.FltAtm p, A arg) { return visitDefault(p, arg); }
    public R visitDefault(com.biosimilarity.lift.lib.term.Prolog.Absyn.Atom p, A arg) {
      throw new IllegalArgumentException(this.getClass().getName() + ": " + p);
    }
/* Functor */
    public R visit(com.biosimilarity.lift.lib.term.Prolog.Absyn.FAtm p, A arg) { return visitDefault(p, arg); }
    public R visitDefault(com.biosimilarity.lift.lib.term.Prolog.Absyn.Functor p, A arg) {
      throw new IllegalArgumentException(this.getClass().getName() + ": " + p);
    }
/* Boole */
    public R visit(com.biosimilarity.lift.lib.term.Prolog.Absyn.Verity p, A arg) { return visitDefault(p, arg); }
    public R visit(com.biosimilarity.lift.lib.term.Prolog.Absyn.Absurdity p, A arg) { return visitDefault(p, arg); }
    public R visitDefault(com.biosimilarity.lift.lib.term.Prolog.Absyn.Boole p, A arg) {
      throw new IllegalArgumentException(this.getClass().getName() + ": " + p);
    }
/* Var */
    public R visit(com.biosimilarity.lift.lib.term.Prolog.Absyn.V p, A arg) { return visitDefault(p, arg); }
    public R visit(com.biosimilarity.lift.lib.term.Prolog.Absyn.A p, A arg) { return visitDefault(p, arg); }
    public R visitDefault(com.biosimilarity.lift.lib.term.Prolog.Absyn.Var p, A arg) {
      throw new IllegalArgumentException(this.getClass().getName() + ": " + p);
    }
/* Lyst */
    public R visit(com.biosimilarity.lift.lib.term.Prolog.Absyn.Empty p, A arg) { return visitDefault(p, arg); }
    public R visit(com.biosimilarity.lift.lib.term.Prolog.Absyn.Enum p, A arg) { return visitDefault(p, arg); }
    public R visit(com.biosimilarity.lift.lib.term.Prolog.Absyn.Cons p, A arg) { return visitDefault(p, arg); }
    public R visit(com.biosimilarity.lift.lib.term.Prolog.Absyn.ConsV p, A arg) { return visitDefault(p, arg); }
    public R visitDefault(com.biosimilarity.lift.lib.term.Prolog.Absyn.Lyst p, A arg) {
      throw new IllegalArgumentException(this.getClass().getName() + ": " + p);
    }

}
