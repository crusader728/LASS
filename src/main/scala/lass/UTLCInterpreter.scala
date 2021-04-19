package lass

trait UTLCInterpreter[F[_]] {
  def executeLine(command: Command): F[String]
}
