package lass

import fastparse._, NoWhitespace._
import FastParse._
import cats.MonadError

final class FastParseUTLCModule[F[_]](implicit F: MonadError[F, String]) extends UTLCParser[F] {

  def parseUntypedLambdaCalculus(line: String): F[Expression] = {
    parse(line, parseExpression(_)) match {
      case f: Parsed.Failure => F.raiseError(f.msg)
      case Parsed.Success(value, index) => F.pure(value)
    }
  }
}
