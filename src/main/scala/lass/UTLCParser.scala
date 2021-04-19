package lass

trait UTLCParser[F[_]] {
  def parseUntypedLambdaCalculus(line: String): F[Expression]
}
