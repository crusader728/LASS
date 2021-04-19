import cats.Monad


import lass.Reducer._
import lass.Syntaxs._
import lass.FastParseUTLCModule
import lass.FastParseCommandModule
import cats.data.StateT
import lass.StateEvaluationModule
import lass.Interpreter._
import lass.Application
import lass.Evaluate
import lass.Variable
import lass.Abstraction
import lass.LambdaVar
import lass.Define
import lass.UTLCInterpreterModule
import lass.EnvironmentVar
import cats.implicits._
import cats.data.IdT

import fastparse._, NoWhitespace._

type ErrorOr[A] = Either[String, A]

val parser = new FastParseUTLCModule[ErrorOr]

parser.parseUntypedLambdaCalculus("(\\f.(\\s.(f (s s)) \\s.(f (s s))) (\\x.x))")

val commandParser = new FastParseCommandModule[ErrorOr]

parser.parseUntypedLambdaCalculus("x123")

parser.parseUntypedLambdaCalculus("\\x y z.z")

parser.parseUntypedLambdaCalculus("\\x.x b s e v")

parser.parseUntypedLambdaCalculus("(\\x.x \\y.y )")

parser.parseUntypedLambdaCalculus("((\\x.x true))")

parser.parseUntypedLambdaCalculus("\\a b.a")

parser.parseUntypedLambdaCalculus("(\\f.(\\s.(f (s s)) \\s.(f (s s))) (\\x.x))")

commandParser.readLine("x := (f w)")

commandParser.readLine(":print true")

commandParser.readLine("(\\f.(\\s.(f (s s)) \\s.(f (s s))) (\\x.x))")

vars(parser.parseUntypedLambdaCalculus("(\\f.(\\s.(f (s s)) \\s.(f (s s))) (\\x.x))").getOrElse(null))

freeVars(parser.parseUntypedLambdaCalculus("\\x.\\y.(z z)").getOrElse(null))

boundVars(parser.parseUntypedLambdaCalculus("\\x.\\y.(z z)").getOrElse(null))

alphaEquiv(parser.parseUntypedLambdaCalculus("\\x.x").getOrElse(null), parser.parseUntypedLambdaCalculus("\\y.y").getOrElse(null))

betaReduction(0, parser.parseUntypedLambdaCalculus("(\\x.x (\\y.y c))").getOrElse(null))

betaNormalForm(0, parser.parseUntypedLambdaCalculus("(\\x.x \\y.y)").getOrElse(null))

type TM[A] = StateT[Attempt, Environment, A]
val evaulation = new StateEvaluationModule[TM]()

evaulation.evalDefine("zero", parser.parseUntypedLambdaCalculus("((\\x.x))").getOrElse(null)).runS(Map())
evaulation.evalExpression(EnvironmentVar("abc")).runS(Map())

val s = "λ"

s.replace("λ", "\\")

val interpreter = new UTLCInterpreterModule[TM](evaulation)
// for {
//   (env1, l1) <- interpreter.executeLine(Define("True", Abstraction(LambdaVar('x', 0), Abstraction(LambdaVar('y', 0), Variable(LambdaVar('x', 0)))))).run(Map())
//   (env2, l2) <- interpreter.executeLine(Evaluate(Application(Abstraction(LambdaVar('x', 0), Variable(LambdaVar('x', 0))), EnvironmentVar("True")))).run(env1)
// } yield l1 ++ l2

interpreter.executeLine(Define("True", Abstraction(LambdaVar('x', 0), Abstraction(LambdaVar('y', 0), Variable(LambdaVar('x', 0)))))).run(Map()).flatMap {case (e1, l1) => {
  interpreter.executeLine(Evaluate(Application(Abstraction(LambdaVar('x', 0), Variable(LambdaVar('x', 0))), EnvironmentVar("True")))).run(e1).map {case (e2, l2) => {
    l1 ++ l2
  }}
}}

// convertToNames(false ,false , Variable(LambdaVar('.', 0)), env, Abstraction(LambdaVar('x', 0), Abstraction(LambdaVar('y', 0), Variable(LambdaVar('x', 0)))))

