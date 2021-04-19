package lass

import fastparse._, NoWhitespace._

object FastParse {
  def identifier[_: P] = {
    def firstLetter = P(CharIn("a-z"))
    def rest = P( (CharIn("a-zA-Z0-9_")).rep(0) )
    P( (firstLetter ~ rest).! )
  }

  def whitespace[_: P] = P( CharsWhileIn(" \t", 0) )

  def openParenthesis[_: P] = P("(")

  def closeParenthesis[_: P] = P(")")

  def assign[_: P] = P(":=")

  def letter[_: P] = P( CharIn("a-z") )

  def lambda[_: P] = P("\\")

  def dot[_: P] = P(".")

  def parseVariable[_: P] = identifier.map(str => {
      if(str.size == 1) {
        Variable(LambdaVar(str(0), 0))
      } else {
        EnvironmentVar(str)
      }
  })

  def parseAbstraction[_: P] = {
      def variables = letter.!.rep(min = 1, sep = whitespace)
      def uncurry(l: List[String], e: Expression): Expression = l match {
          case Nil => e
          case x :: xs => Abstraction(LambdaVar(x(0), 0), uncurry(xs, e))
      }
      P (for {
          _ <- lambda
          _ <- whitespace
          xs <- variables
          _ <- dot
          _ <- whitespace
          e <- parseExpression
      } yield uncurry(xs.toList, e))
  }

  def parseApplicationBase[_: P]: P[Expression] = for {
      e1 <- parseSingleton
      _ <- whitespace
      e2 <- parseSingleton
  } yield Application(e1, e2)

  def parseApplicationTail[_: P] = P( (whitespace ~ parseSingleton).rep )

  def parseApplication[_: P]: P[Expression] = for {
      application <- parseApplicationBase
      es <- parseApplicationTail
  } yield es.foldLeft(application) {case (acc, e) => Application(acc, e)}

  def parseParenthesis[_: P] = P(openParenthesis ~ whitespace ~ ( parseApplication | parseSingleton ) ~ whitespace ~ closeParenthesis)

  def parseSingleton[_: P]: P[Expression] = parseVariable | parseAbstraction | parseParenthesis

  def parseExpression[_: P]: P[Expression] = (parseSingleton | parseApplication)

  def parseDefine[_: P]: P[Command] = (for{
      variable <- identifier
      _ <- whitespace
      _ <- assign
      _ <- whitespace
      e <- parseExpression
  } yield Define(variable, e))

  def parseEvaluate[_: P]: P[Command] = parseExpression.map(Evaluate(_))

  def parsePrint[_: P]: P[Command] = P(P(":print") ~ whitespace ~ identifier.!).map(Print(_))

  def parseCommand[_: P] = P( (parseDefine | parseEvaluate | parsePrint) ~ End )
}
