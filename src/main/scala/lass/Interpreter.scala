package lass

import Reducer._
import Syntaxs._
import cats.Show
import cats.kernel.Eq
import scala.collection.mutable.ListBuffer
import scala.annotation.tailrec
import cats.syntax.show

object Interpreter {
  def convertToName(env: Environment, e: Expression): Attempt[String] = {
    env.find { case (n, exp) => alphaEquiv(exp, e) } match {
      case Some(value) => Right(value._1)
      case None => Left("not found")
    }
  }

  def convertToNames(redexFound: Boolean, 
  redexVarFound: Boolean, 
  redexVar: Expression, 
  env: Environment, 
  exp: Expression)(implicit 
  expressionEqual: Eq[Expression],
  lambdaVarShow: Show[LambdaVar]
  ): String = {
    exp match {
      case v@Variable(name) => lambdaVarShow.show(name)
      case abs@Abstraction(variable, expression) => {
        convertToName(env, abs) match {
          case Left(_) => s"(λ${lambdaVarShow.show(variable)}.${convertToNames(redexFound, redexVarFound, redexVar, env, expression)})"
          case Right(value) => value
        }
      }
      case redex@Application(abs@Abstraction(v1, e1), e2) => {
        if(redexFound) {
          convertToName(env, redex) match {
            case Left(_) => s"(${convertToNames(true, false, redexVar, env, abs)} ${convertToNames(true, false, redexVar, env, e2)})"
            case Right(value) => value
          }
        } else {
          s"(λ${lambdaVarShow.show(v1)}.(${convertToNames(true, true, Variable(v1), env, e1)}) ${convertToNames(true, false, redexVar, env, e2)})"
        }
      }
      case app@Application(e1, e2) => {
        convertToName(env, app) match {
          case Left(_) => s"(${convertToNames(redexFound, redexVarFound, redexVar, env, e1)} ${convertToNames(redexFound, redexVarFound, redexVar, env, e2)})"
          case Right(value) => value
        }
      }
    }
  }

  def showSummary(env: Environment, expression: Expression, step: Int)(implicit expressionShow: Show[Expression]): Summary = {
    Summary(step, expressionShow.show(expression), convertToNames(false, false, Variable(LambdaVar('.', 0)), env, expression))
  }

  def showResult(env: Environment, expression: Expression, steps: Int): Summary = {
    val(f, s) = betaNormalForm(steps, expression)
    showSummary(env, f, if(s == 0) steps else s)
  }

  def reduce(env: Environment, exp: Expression, num: Int)(implicit summaryShow: Show[Summary]): (Expression, List[String]) = {
    @tailrec
    def helper(expression: Expression, step: Int, accmulate: ListBuffer[String]): (Expression, Int, List[String]) = {
      accmulate.append(s"${step} #: ${convertToNames(false, false, Variable(LambdaVar('.', 0)), env, expression)}")
      val candidates = betaReductionCandidates(expression)
      if(!candidates.isEmpty) {
        val (f, s) = betaReduction(step, expression)
        helper(f, s, accmulate)
      } else {
        (expression, step, accmulate.toList)
      }
    } 

    val (e, step, log) = helper(exp, num, ListBuffer())
    (e, log appended summaryShow.show(showSummary(env, e, step)))
  }
}
