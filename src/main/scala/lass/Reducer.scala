package lass

import Syntaxs._
import cats.kernel.Eq

object Reducer {
  def fold[A](
    f: LambdaVar => A => A,
    g: A => A => A,
    h: LambdaVar => A,
    e: Expression): A = e match {
        case Variable(n) => h(n)
        case Abstraction(v, e1) => f(v)(fold(f, g, h, e1))
        case Application(e1, e2) => g(fold(f, g, h, e1))(fold(f, g, h, e2))
  }   

  def vars(e: Expression): Set[LambdaVar] = fold(
    (v: LambdaVar) => (s: Set[LambdaVar]) => s + v,
    (s1: Set[LambdaVar]) => (s2: Set[LambdaVar]) => s1.union(s2),
    (v: LambdaVar) => Set[LambdaVar](v),
    e
  )

  def freeVars(e: Expression): Set[LambdaVar] = fold(
    (v: LambdaVar) => (s: Set[LambdaVar]) => s - v,
    (s1: Set[LambdaVar]) => (s2: Set[LambdaVar]) => s1.union(s2),
    (v: LambdaVar) => Set[LambdaVar](v),
    e
  )

  def boundVars(e: Expression): Set[LambdaVar] = vars(e) -- freeVars(e)

  //substitutes all free occurences of x by n in e
  def substitute(x: LambdaVar, n: Expression, e: Expression)(implicit lambdaVarEq: Eq[LambdaVar]): Expression = e match {
    case v@Variable(y) => if(lambdaVarEq.eqv(x, y)) n else v
    case Application(p, q) => Application(substitute(x, n, p), substitute(x, n, q))
    case lambda@Abstraction(y, e1) => {
      val free1 = freeVars(e1)
      val free2 = freeVars(n)
      if(lambdaVarEq.eqv(x, y)) {
        lambda
      } else if(free1(x) == false) {
        lambda
      } else if(free2(y) == false) {
        Abstraction(y, substitute(x, n, e1))
      } else {
        val newVar = LambdaVar(y.name, y.index + 1)
        Abstraction(newVar, substitute(x, n, substitute(y, Variable(newVar), e1)))
      }
    }
  }

  def alphaEquiv(e1: Expression, e2: Expression)(implicit expressionEqual: Eq[Expression], lambdaVarEq: Eq[LambdaVar]): Boolean = {
    (e1, e2) match {
      case (v1: Variable, v2: Variable) => expressionEqual.eqv(v1, v2)
      case (Application(e1, e2), Application(e3, e4)) => alphaEquiv(e1, e3) && alphaEquiv(e2, e4)
      case (Abstraction(v1, Variable(v2)), Abstraction(v3, Variable(v4))) => if(lambdaVarEq.eqv(v1, v2) && !lambdaVarEq.eqv(v3, v4)) {
        false
      } else if(!lambdaVarEq.eqv(v1, v2) && lambdaVarEq.eqv(v3, v4)) {
        false
      } else {
        true
      }
      case (Abstraction(v1, e1), Abstraction(v2, e2)) => alphaEquiv(e1, substitute(v2, Variable(v1), e2))
      case _ => false
    }
  }

  def betaReductionCandidates(e: Expression): List[Expression] = e match {
    case Variable(name) => Nil
    case app@Application(lambda@Abstraction(_, e1), e2) => app :: (betaReductionCandidates(e1) ++ betaReductionCandidates(e2))
    case Application(e1, e2) => betaReductionCandidates(e1) ++ betaReductionCandidates(e2)
    case Abstraction(_, e1) => betaReductionCandidates(e1)
  }

  def betaReduction(step: Int, expression: Expression): (Expression, Int) = expression match {
    case v@Variable(_) => (v, step)
    case Abstraction(v, e) => {
      val (newE, newStep) = betaReduction(step, e)
      (Abstraction(v, newE), newStep)
    }
    case Application(Abstraction(v, e1), e2) => (substitute(v, e2, e1), step + 1)
    case Application(e1, e2) => {
      val candidates = betaReductionCandidates(e1)
      if(!candidates.isEmpty) {
        val (newE, newStep) = betaReduction(step, e1)
        (Application(newE, e2), newStep)
      } else {
        val (newE, newStep) = betaReduction(step, e2)
        (Application(e1, newE), newStep)
      }
    } 
  }

  def betaNormalForm(step: Int, expression: Expression): (Expression, Int) = {
    val candidates = betaReductionCandidates(expression)
    if(candidates.isEmpty) {
      (expression, step)
    } else {
      val (newE, newStep) = betaReduction(step, expression)
      betaNormalForm(newStep, newE)
    }
  }
}
