package lass

trait Evaluation[F[_]] {
    def evalExpression(e: Expression): F[Expression]
    def evalDefine(name: String, e: Expression): F[Expression]
}
