package lass

import cats.kernel.Order
import cats.kernel.Eq
import cats.Show
import cats.data.StateT


final case class LambdaVar(name: Char, index: Int) extends Product with Serializable

sealed abstract class Expression extends Product with Serializable
final case class Variable(name: LambdaVar) extends Expression
final case class Abstraction(variable: LambdaVar, expression: Expression) extends Expression
final case class Application(e1: Expression, e2: Expression) extends Expression
final case class EnvironmentVar(name: String) extends Expression

sealed abstract class Command extends Product with Serializable
final case class Define(name: String, e: Expression) extends Command
final case class Evaluate(e: Expression) extends Command
final case class Print(name: String) extends Command

final case class Summary(reductionCount: Int, betaNF: String, alphaEquiv: String)


object Syntaxs {

  implicit val lambdaVarOrdering: Ordering[LambdaVar] = Ordering.by { l => (l.name, l.index) }

  implicit val lambdaVarEq: Eq[LambdaVar] = (l1, l2) => l1 == l2

  implicit val lambdaVarOrder: Order[LambdaVar] = (l1, l2) => {
      val ordering = implicitly[Ordering[LambdaVar]]
      ordering.compare(l1, l2)
  }

  def showVarHelper(i: Int): String = {
    @scala.annotation.tailrec
    def helper(x: Int, acc: List[Char]): List[Char] = {
      x match {
        case 0 => acc
        case _ => helper(x - 1, '\'' :: acc)
      }
    }  

    helper(i, Nil).mkString
  }

  implicit val lambdaVarShow: Show[LambdaVar] = new Show[LambdaVar] {
    override def show(t: LambdaVar): String = if(t.index == 0) {
      s"${t.name}"
    } else {
      s"${t.name}${showVarHelper(t.index).mkString}"
    }

  }

  private def uncurryShow(exp: Expression): String = {
    val ls = implicitly[Show[LambdaVar]]
    val es = implicitly[Show[Expression]]

    exp match {
      case Abstraction(v1, Abstraction(v2, e)) => List(ls.show(v1), ls.show(v2), uncurryShow(e)).mkString
      case Abstraction(v, e) => ls.show(v) ++ "." ++ es.show(e)
      case Variable(v) => List(". ", ls.show(v)).mkString
      case Application(e1, e2) => List(".", es.show(e1), es.show(e2)).mkString(" ")
      case EnvironmentVar(n) => n
    }
  }

  implicit def expressionShow(implicit lambdaVarShow: Show[LambdaVar]): Show[Expression] = new Show[Expression] {
    def show(e: Expression): String = e match {
      case Variable(name) => lambdaVarShow.show(name)
      case abs@(Abstraction(_, _)) => "(λ" + uncurryShow(abs) + ")"
      case Application(e1, e2) => "(" ++ this.show(e1) ++ " " ++ this.show(e2) ++ ")"
      case EnvironmentVar(name) => name
    }
  }

  implicit def expressionEqual(implicit lambdaVarEq: Eq[LambdaVar]): Eq[Expression] = new Eq[Expression] {
    def eqv(a1: Expression, a2: Expression): Boolean = (a1, a2) match {
      case (Variable(v1), Variable(v2)) => lambdaVarEq.eqv(v1, v2)
      case (Abstraction(v1, e1), Abstraction(v2, e2)) => lambdaVarEq.eqv(v1, v2) && this.eqv(e1, e2)
      case (Application(e1, e2), Application(e3, e4)) => this.eqv(e1, e3) && this.eqv(e2, e4)
      case (EnvironmentVar(n1), EnvironmentVar(n2)) => n1 == n2
      case _ => false
    }
  }

  implicit def commandEqual(implicit expressionEqual: Eq[Expression]): Eq[Command] = new Eq[Command] {
    override def eqv(a1: Command, a2: Command): Boolean = (a1, a2) match {
      case (Define(n1, e1), Define(n2, e2)) => n1 == n2 && expressionEqual.eqv(e1, e2)
      case (Evaluate(e1), Evaluate(e2)) => expressionEqual.eqv(e1, e2)
      case (Print(n1), Print(n2)) => n1 == n2
      case _ => false
    }
  }

  implicit val summaryShow: Show[Summary] = new Show[Summary] {
    override def show(t: Summary): String = List(s"total steps: ${t.reductionCount}", s"uncurried beta normal form: ${t.betaNF}", s"alpha equivlant: ${t.alphaEquiv}").mkString("\n")
  }

  type Environment = Map[String, Expression]
  type UTLCError = String
  type Attempt[A] = Either[UTLCError, A]
  type StateFulAttempt[A] = StateT[Attempt, Environment, A]
}
