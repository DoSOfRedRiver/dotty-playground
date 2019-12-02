package predef

case class EitherT[F[_], A, B](value: F[Either[A, B]])

given [F[_]: Monad, L]: Monad[[X] =>> EitherT[F, L, X]] {
  def[A,B] (fa: EitherT[F, L, A]) flatMap (f: A => EitherT[F, L, B]): EitherT[F, L, B] = {
    EitherT(
      fa.value flatMap {
        case Left(l) => 
          summon[Monad[F]].pure(Left(l))
        case Right(a) =>
          f(a).value
      }
    )
  }

  def[A] (a: A) pure: EitherT[F, L, A] = 
    EitherT(summon[Monad[F]].pure(Right(a)))
}