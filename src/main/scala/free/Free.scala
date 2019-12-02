package free

import predef.{Monad, Functor, StackSafeMonad, TailRec, Applicative}
import scala.annotation.tailrec


enum Free[F[_], A] {
  case Pure(a: A)
  case Suspend(fa: F[A])
  case FlatMap[F[_], A, B](p: Free[F, A], f: A => Free[F, B]) extends Free[F, B]

  import Free.roll

  def resume(given Functor[F]): Either[F[Free[F, A]], A] = this match {
    case Pure(a) =>
      Right(a)
    case Suspend(cont) =>
      Left(cont map Pure.apply)
    case FlatMap(p, f) =>
      p match {
        case Pure(a) =>
          f(a).resume
        case Suspend(cont) =>
          Left(cont map f)
        case FlatMap(p1, f1) =>
          p1.flatMap(f1(_) flatMap f).resume
      }
  }

  def eval(f: F[Free[F, A]] => Free[F, A])(given Functor[F]): A = {
    @tailrec
    def loop(free: Free[F, A]): A = {
      free.resume match {
        case Right(a) => a
        case Left(step) => 
          loop(f(step))
      }
    }

    loop(this)
  }

  def step: Free[F, A] = this match {
    case FlatMap(FlatMap(cont, f), g) =>
      cont.flatMap(cont1 => f(cont1).flatMap(g)).step
    case FlatMap(Pure(a), f) =>
      f(a).step
    case x => x
  }

  def zip[B](fb: Free[F, B])(given Functor[F]): Free[F, (A,B)] = {
    (resume, fb.resume) match {
      case (Left(fa), Left(fb)) =>
        fa.roll zip fb.roll
      case (Left(fa), Right(b)) =>
        fa.roll zip Pure(b)
      case (Right(a), Left(fb)) =>
        Pure(a) zip fb.roll
      case (Right(a), Right(b)) =>
        Pure((a,b))
    }
  }

  def foldMap[M[_]: Applicative: TailRec](f: [X] => F[X] => M[X]): M[A] = {
    summon[TailRec[M]].tailRecM(this, _.step match {
      case Pure(a) => Right(a).pure
      case Suspend(cont) =>
        f(cont) map Right.apply
      case FlatMap(prev, g) =>
        prev.foldMap(f).map(cont => Left(g(cont)))
    })
  }
}

given FreeMonad[F[_]]: Monad[[X] =>> Free[F, X]] {
  def[A,B] (fa: Free[F, A]) flatMap (f: A => Free[F, B]): Free[F, B] =
    Free.FlatMap(fa, f)

  def[A] (a: A) pure = Free.Pure(a)
}

object Free {
  def liftF[F[_], A](fa: F[A]): Free[F, A] = {
    Suspend(fa)
  }

  def[F[_], A] (value: F[Free[F, A]]) roll: Free[F, A] =
    liftF(value).flatMap(identity)

  def defer[F[_], A](a: => Free[F, A]): Free[F, A] = {
    //This is temporary to avoid bug in the compiler
    (summon[Monad[[X] =>> Free[F, X]]].pure(())) flatMap (_ => a)
  }
}