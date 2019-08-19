package predef

trait Functor[F[_]] extends FunctorSyntax[F] {
  def (fa: F[A]) map[A,B] (f: A => B): F[B]
}

trait Applicative[F[_]] extends Functor[F] {
  def (fa: F[A]) ap[A,B] (fab: F[A => B]): F[B]
  def (a: A) pure[A]: F[A]
}

trait Monad[F[_]] extends Applicative[F] with MonadSyntax[F] {
  def (fa: F[A]) flatMap[A,B] (f: A => F[B]): F[B]

  def (fa: F[A]) ap[A,B] (fab: F[A => B]): F[B] = {
      for {
        a <- fa
        f <- fab
      } yield f(a)
    }

  def (fa: F[A]) map[A,B] (f: A => B): F[B] = fa flatMap (f andThen pure)
}

trait MonadSyntax[F[_]] {
  self: Monad[F] =>

  def (ffa: F[F[A]]) flatten[A]: F[A] = ffa.flatMap(x => x)
  
  def (fa: F[A]) >> [A,B](fb: => F[B]): F[B] = {
    fa flatMap (_ => fb)
  }

  def (a: A) ret [A]: F[A] = {
    self.pure(a)
  }
}

trait FunctorSyntax[F[_]] {
  self: Functor[F] =>
  
  def (fa: F[A]) void[A]: F[Unit] = fa map (_ => ())
}

trait TailRec[F[_]] {
  def tailRecM[A, B](a: A, f: A => F[Either[A, B]]): F[B]
}

trait StackSafeMonad[F[_]] extends TailRec[F] with Monad[F] {
  def tailRecM[A, B](a: A, f: A => F[Either[A, B]]): F[B] = flatMap(f(a)) {
    case Left(a)  => tailRecM(a, f)
    case Right(b) => pure(b)
  }
}

delegate StackSafeListMonad for StackSafeMonad[List] {
  def (fa: List[A]) flatMap[A,B] (f: A => List[B]): List[B] = {
    fa.flatMap(f)
  }

  def (a: A) pure[A]: List[A] = List(a)
}