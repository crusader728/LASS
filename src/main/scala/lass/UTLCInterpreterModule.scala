package lass

import cats.MonadError
import cats.mtl.Stateful
import cats.Show
import Syntaxs._
import cats.Monad
import cats.implicits._
import cats.mtl._
import Interpreter._

final class UTLCInterpreterModule[M[_]](val evaulation: Evaluation[M])
(implicit M: MonadError[M, String], S: Stateful[M, Environment], expressionShow: Show[Expression])
extends UTLCInterpreter[M] {

  override def executeLine(command: Command): M[String] = {
    command match {
      case Define(name, e) => evaulation.evalDefine(name, e).map(_ => s"${name} defined")
      case Evaluate(e) => for {
        env <- S.get
        newExpression <- evaulation.evalExpression(e)
        (_, log) = reduce(env, newExpression, 0)
      } yield log.mkString("\n")
      case Print(name) => evaulation.evalExpression(EnvironmentVar(name)).map(e => expressionShow.show(e))
    }
  }
}
