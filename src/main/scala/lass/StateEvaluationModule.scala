package lass

import Syntaxs.Environment
import cats.MonadError
import cats.mtl.Stateful
import cats.implicits._
import cats.Monad

final class StateEvaluationModule[M[_]]
(implicit M: MonadError[M, String], S: Stateful[M, Environment])
extends Evaluation[M] {

  override def evalExpression(e: Expression): M[Expression] = e match {
    case v@Variable(_) => M.pure(v)
    case Abstraction(v, e) => evalAbstraction(v, e)
    case Application(e1, e2) => evalApplication(e1, e2)
    case EnvironmentVar(name) => evalVariable(name)
  }
  override def evalDefine(name: String, e: Expression): M[Expression] = for {
    newE <- evalExpression(e)
    _ <- S.modify(env => env + (name -> newE))
  } yield newE

  private def evalVariable(name: String): M[Expression] = {
    for {
      env <- S.get
      e <- env.get(name) match {
          case None => M.raiseError(s"$name undeclared")
          case Some(v) => M.pure(v)
      }
    } yield e
  }

  private def evalAbstraction(variable: LambdaVar, e: Expression): M[Expression] = for {
    e1 <- evalExpression(e)
  } yield Abstraction(variable, e1)

  private def evalApplication(e1: Expression, e2: Expression): M[Expression] = for{
    f <- evalExpression(e1)
    x <- evalExpression(e2)
  } yield Application(f, x)
}


