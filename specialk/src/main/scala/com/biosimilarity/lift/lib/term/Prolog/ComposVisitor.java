package com.biosimilarity.lift.lib.term.Prolog;
import com.biosimilarity.lift.lib.term.Prolog.Absyn.*;
/** BNFC-Generated Composition Visitor
*/

public class ComposVisitor<A> implements
  com.biosimilarity.lift.lib.term.Prolog.Absyn.Predicate.Visitor<com.biosimilarity.lift.lib.term.Prolog.Absyn.Predicate,A>,
  com.biosimilarity.lift.lib.term.Prolog.Absyn.Term.Visitor<com.biosimilarity.lift.lib.term.Prolog.Absyn.Term,A>,
  com.biosimilarity.lift.lib.term.Prolog.Absyn.Atom.Visitor<com.biosimilarity.lift.lib.term.Prolog.Absyn.Atom,A>,
  com.biosimilarity.lift.lib.term.Prolog.Absyn.Functor.Visitor<com.biosimilarity.lift.lib.term.Prolog.Absyn.Functor,A>,
  com.biosimilarity.lift.lib.term.Prolog.Absyn.Boole.Visitor<com.biosimilarity.lift.lib.term.Prolog.Absyn.Boole,A>,
  com.biosimilarity.lift.lib.term.Prolog.Absyn.Var.Visitor<com.biosimilarity.lift.lib.term.Prolog.Absyn.Var,A>,
  com.biosimilarity.lift.lib.term.Prolog.Absyn.Lyst.Visitor<com.biosimilarity.lift.lib.term.Prolog.Absyn.Lyst,A>
{
/* Predicate */
    public Predicate visit(com.biosimilarity.lift.lib.term.Prolog.Absyn.APred p, A arg)
    {
      Atom atom_ = p.atom_.accept(this, arg);

      return new com.biosimilarity.lift.lib.term.Prolog.Absyn.APred(atom_);
    }
    public Predicate visit(com.biosimilarity.lift.lib.term.Prolog.Absyn.CPred p, A arg)
    {
      Functor functor_ = p.functor_.accept(this, arg);
      ListTerm listterm_ = new ListTerm();
      for (Term x : p.listterm_) {
        listterm_.add(x.accept(this,arg));
      }

      return new com.biosimilarity.lift.lib.term.Prolog.Absyn.CPred(functor_, listterm_);
    }

/* Term */
    public Term visit(com.biosimilarity.lift.lib.term.Prolog.Absyn.TAtom p, A arg)
    {
      Atom atom_ = p.atom_.accept(this, arg);

      return new com.biosimilarity.lift.lib.term.Prolog.Absyn.TAtom(atom_);
    }
    public Term visit(com.biosimilarity.lift.lib.term.Prolog.Absyn.VarT p, A arg)
    {
      Var var_ = p.var_.accept(this, arg);

      return new com.biosimilarity.lift.lib.term.Prolog.Absyn.VarT(var_);
    }
    public Term visit(com.biosimilarity.lift.lib.term.Prolog.Absyn.Complex p, A arg)
    {
      Functor functor_ = p.functor_.accept(this, arg);
      ListTerm listterm_ = new ListTerm();
      for (Term x : p.listterm_) {
        listterm_.add(x.accept(this,arg));
      }

      return new com.biosimilarity.lift.lib.term.Prolog.Absyn.Complex(functor_, listterm_);
    }
    public Term visit(com.biosimilarity.lift.lib.term.Prolog.Absyn.TList p, A arg)
    {
      Lyst lyst_ = p.lyst_.accept(this, arg);

      return new com.biosimilarity.lift.lib.term.Prolog.Absyn.TList(lyst_);
    }

/* Atom */
    public Atom visit(com.biosimilarity.lift.lib.term.Prolog.Absyn.Atm p, A arg)
    {
      String lident_ = p.lident_;

      return new com.biosimilarity.lift.lib.term.Prolog.Absyn.Atm(lident_);
    }
    public Atom visit(com.biosimilarity.lift.lib.term.Prolog.Absyn.EAtm p, A arg)
    {
      String ident_ = p.ident_;

      return new com.biosimilarity.lift.lib.term.Prolog.Absyn.EAtm(ident_);
    }
    public Atom visit(com.biosimilarity.lift.lib.term.Prolog.Absyn.BAtm p, A arg)
    {
      Boole boole_ = p.boole_.accept(this, arg);

      return new com.biosimilarity.lift.lib.term.Prolog.Absyn.BAtm(boole_);
    }
    public Atom visit(com.biosimilarity.lift.lib.term.Prolog.Absyn.StrAtm p, A arg)
    {
      String string_ = p.string_;

      return new com.biosimilarity.lift.lib.term.Prolog.Absyn.StrAtm(string_);
    }
    public Atom visit(com.biosimilarity.lift.lib.term.Prolog.Absyn.IntAtm p, A arg)
    {
      Integer integer_ = p.integer_;

      return new com.biosimilarity.lift.lib.term.Prolog.Absyn.IntAtm(integer_);
    }
    public Atom visit(com.biosimilarity.lift.lib.term.Prolog.Absyn.FltAtm p, A arg)
    {
      Double double_ = p.double_;

      return new com.biosimilarity.lift.lib.term.Prolog.Absyn.FltAtm(double_);
    }

/* Functor */
    public Functor visit(com.biosimilarity.lift.lib.term.Prolog.Absyn.FAtm p, A arg)
    {
      String lident_ = p.lident_;

      return new com.biosimilarity.lift.lib.term.Prolog.Absyn.FAtm(lident_);
    }

/* Boole */
    public Boole visit(com.biosimilarity.lift.lib.term.Prolog.Absyn.Verity p, A arg)
    {

      return new com.biosimilarity.lift.lib.term.Prolog.Absyn.Verity();
    }
    public Boole visit(com.biosimilarity.lift.lib.term.Prolog.Absyn.Absurdity p, A arg)
    {

      return new com.biosimilarity.lift.lib.term.Prolog.Absyn.Absurdity();
    }

/* Var */
    public Var visit(com.biosimilarity.lift.lib.term.Prolog.Absyn.V p, A arg)
    {
      String uident_ = p.uident_;

      return new com.biosimilarity.lift.lib.term.Prolog.Absyn.V(uident_);
    }
    public Var visit(com.biosimilarity.lift.lib.term.Prolog.Absyn.A p, A arg)
    {
      String wild_ = p.wild_;

      return new com.biosimilarity.lift.lib.term.Prolog.Absyn.A(wild_);
    }

/* Lyst */
    public Lyst visit(com.biosimilarity.lift.lib.term.Prolog.Absyn.Empty p, A arg)
    {

      return new com.biosimilarity.lift.lib.term.Prolog.Absyn.Empty();
    }
    public Lyst visit(com.biosimilarity.lift.lib.term.Prolog.Absyn.Enum p, A arg)
    {
      ListTerm listterm_ = new ListTerm();
      for (Term x : p.listterm_) {
        listterm_.add(x.accept(this,arg));
      }

      return new com.biosimilarity.lift.lib.term.Prolog.Absyn.Enum(listterm_);
    }
    public Lyst visit(com.biosimilarity.lift.lib.term.Prolog.Absyn.Cons p, A arg)
    {
      ListTerm listterm_ = new ListTerm();
      for (Term x : p.listterm_) {
        listterm_.add(x.accept(this,arg));
      }
      Lyst lyst_ = p.lyst_.accept(this, arg);

      return new com.biosimilarity.lift.lib.term.Prolog.Absyn.Cons(listterm_, lyst_);
    }
    public Lyst visit(com.biosimilarity.lift.lib.term.Prolog.Absyn.ConsV p, A arg)
    {
      ListTerm listterm_ = new ListTerm();
      for (Term x : p.listterm_) {
        listterm_.add(x.accept(this,arg));
      }
      Var var_ = p.var_.accept(this, arg);

      return new com.biosimilarity.lift.lib.term.Prolog.Absyn.ConsV(listterm_, var_);
    }

}