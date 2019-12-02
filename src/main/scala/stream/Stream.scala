package stream

import predef._
import predef.given

enum Stream[+F[_], +A] {
  case Emit(elem: A, tail: F[Stream[F, A]])
  case Suspend(rest: F[Stream[F, A]])
  case Done(ex: Option[Exception]) extends Stream[Nothing, Nothing]

  import Stream.done

  def take[F1[x] >: F[x]](n: Int)(given Functor[F1]): Stream[F1, A] = {
    this match {
      case Emit(e, t) =>
        if (n > 0)
          Emit(e, t.map(_.take(n - 1)))
        else
          done
      case Suspend(rest) =>
        Suspend(rest.map(_.take(n)))
      case Done(ex) =>
        Done(ex)
    }
  }

  def ++[F1[x] >: F[x]: Functor, A1 >: A] (s1: => Stream[F1, A1]): Stream[F1, A1] = 
    this match {
      case Emit(elem, tail) =>
        Emit(elem, tail.map(_ ++ s1))
      case Suspend(rest) =>
        Suspend(rest.map(_ ++ s1))
      case Done(Some(ex)) =>
        Done(Some(ex))
      case Done(None) =>
        s1
    }

  def flatMap[F1[x] >: F[x]: Functor, B](f: A => Stream[F1, B]): Stream[F1, B] =
    this match {
      case Emit(elem, tail) =>
        Suspend(tail.map(x => f(elem) ++ x.flatMap(f)))
      case Suspend(rest) =>
        Suspend(rest.map(_.flatMap(f)))
      case Done(ex) =>
        Done(ex)
    }

  def evalMap[F1[x] >: F[x]: Monad, B](f: A => F1[B]): Stream[F1, B] = {
    flatMap { a =>
      Stream.eval(f(a))
    }
  }

  def evalTap[F1[x] >: F[x]: Monad](f: A => F1[Unit]): Stream[F1, A] = {
    evalMap { a =>
      f(a) as a
    }
  }

  def repeat[F1[x] >: F[x]: Functor]: Stream[F1, A] = {
    this ++ repeat
  }

  def foldRight[F1[x] >: F[x]: Monad: Raise, B](zero: B, f: (A,B) => B): F1[B] = this match {
    case Done(None) =>
      zero.ret
    case Done(Some(ex)) =>
      ex.raise
    case Suspend(rest) =>
      rest.flatMap(_.foldRight(zero, f))
    case Emit(a, t) =>
      for {
        s <- t
        b <- s.foldRight(zero, f)
      } yield f(a, b)
  }

  def toList[F1[x] >: F[x]: Monad: Raise, A1 >: A]: F1[List[A1]] = 
    foldRight(List.empty, _ :: _)

  def drain[F1[x] >: F[x]: Monad: Raise]: F1[Unit] = 
    foldRight((), (_,_) => ())
}

object Stream {
  def eval[F[_]: Monad, A](fa: F[A]): Stream[F, A] =
    Stream.Suspend(
      fa.map(a =>
        Stream.Emit[F, A](
          a,
          Stream.Done(None).ret,
        )
      )
    )

  def single[F[_]: Monad, A](a: A): Stream[F, A] = {
    eval(a.ret)
  }

  def from[F[_]: Monad](start: Int, step: Int = 1): Stream[F, Int] = {
    single(start) ++ from(start + step, step)
  }

  def done[F[_], A]: Stream[F, A] = Stream.Done(None)
}

given [F[_]: Monad]: Monad[[X] =>> Stream[F, X]] {
  def[A,B] (stream: Stream[F, A]) flatMap (f: A => Stream[F, B]): Stream[F, B] = {
    stream.flatMap(f)
  }

  def[A] (a: A) pure: Stream[F, A] = 
    Stream.single(a)
}