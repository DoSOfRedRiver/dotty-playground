package predef

trait Functor[F[_]] extends FunctorSyntax[F] {
  def[A,B] (fa: F[A]) map (f: A => B): F[B]
}

trait Applicative[F[_]] extends Functor[F] {
  def[A,B] (fa: F[A]) ap (fab: F[A => B]): F[B]
  def[A] (a: A) pure: F[A]
}

trait Monad[F[_]] extends Applicative[F] with MonadSyntax[F] {
  def[A,B] (fa: F[A]) flatMap (f: A => F[B]): F[B]

  def[A,B] (fa: F[A]) ap (fab: F[A => B]): F[B] = {
      for {
        a <- fa
        f <- fab
      } yield f(a)
    }

  def[A,B] (fa: F[A]) map (f: A => B): F[B] = fa flatMap (f andThen pure)
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

given StackSafeListMonad: StackSafeMonad[List] {
  def[A,B] (fa: List[A]) flatMap (f: A => List[B]): List[B] = {
    fa.flatMap(f)
  }

  def[A] (a: A) pure: List[A] = List(a)
}



trait MonadSyntax[F[_]] {
  self: Monad[F] =>

  def[A] (ffa: F[F[A]]) flatten: F[A] = ffa.flatMap(x => x)
  
  def[A,B] (fa: F[A]) >> (fb: => F[B]): F[B] = {
    fa flatMap (_ => fb)
  }

  def[A] (a: A) ret: F[A] = {
    self.pure(a)
  }
}

trait FunctorSyntax[F[_]] {
  self: Functor[F] =>
  
  def[A] (fa: F[A]) void: F[Unit] = fa map (_ => ())

  def[A,B] (fa: F[A]) as (b: B): F[B] = fa map (_ => b)
}