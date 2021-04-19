package lass

import fastparse._, NoWhitespace._
import FastParse._
import cats.MonadError

final class FastParseCommandModule[F[_]]
(implicit F: MonadError[F, String]) extends CommandParser[F] {

  override def readLine(line: String): F[Command] = {
    parse(line, parseCommand(_)) match {
      case f: Parsed.Failure => F.raiseError(f.msg)
      case Parsed.Success(value, index) => F.pure(value)
    }
  }
}
