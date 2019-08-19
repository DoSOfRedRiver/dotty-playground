package predef

case class EitherT[F[_], A, B](value: F[Either[A, B]])

delegate [F[_]: Monad, L] for Monad[[X] =>> EitherT[F, L, X]] {
  def (fa: EitherT[F, L, A]) flatMap[A,B] (f: A => EitherT[F, L, B]): EitherT[F, L, B] = {
    EitherT(
      fa.value flatMap {
        case Left(l) => 
          the[Monad[F]].pure(Left(l))
        case Right(a) =>
          f(a).value
      }
    )
  }

  def (a: A) pure[A]: EitherT[F, L, A] = 
    EitherT(the[Monad[F]].pure(Right(a)))
}