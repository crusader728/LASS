package lass

trait CommandParser[F[_]] {
    def readLine(line: String): F[Command]
}
