package io

import predef.StackSafeMonad
import io.runtime._
import scala.concurrent.ExecutionContext

enum IO[+A] {
    case Pure(a: A)
    case Suspend(thunk: () => A)
    case Error(e: Exception)
    case Map[A, A1](prev: IO[A1], f: A1 => A) extends IO[A]
    case FlatMap[A, A1](prev: IO[A1], f: A1 => IO[A]) extends IO[A]
    case Async[A](cont: Callback[A] => IO[Unit]) extends IO[A]
    case Shift(sc: Scheduler)
    case Fork(cont: IO[A]) extends IO[Fiber[A]]

    def handleErrorWith[B >: A](onError: Exception => IO[B]): IO[B] = {
      FlatMap(this, new ErrorHandler(Pure.apply, onError))
    }

    def handleError[B >: A](onError: Exception => B): IO[B] = {
      FlatMap(this, new ErrorHandler(Pure.apply, onError andThen Pure.apply))
    }

    def fork: IO[Fiber[A]] = Fork(this)
}

object IO {
  def suspend[A](a: => A) =
    Suspend(() => a)

  def asyncF[A](cont: Callback[A] => IO[Unit]): IO[A] =
    Async(cont)

  def async[A](cont: Callback[A] => Unit): IO[A] =
    asyncF[A](cb => suspend(cont(cb)))

  def raise[A](e: Exception): IO[A] =
    IO.suspend(throw e)

  def shift(sc: Scheduler): IO[Unit] = 
    IO.Shift(sc)

  def unit: IO[Unit] = Pure(())

  delegate for StackSafeMonad[IO] {
    def (a: A) pure[A]: IO[A] = IO.Pure(a)

    override def (fa: IO[A]) map[A,B] (f: A => B): IO[B] = {
      fa match {
        case Map(p, g) =>
          Map(p, g andThen f)
        case _ => Map(fa, f)
      }
    }

    def (fa: IO[A]) flatMap[A,B] (f: A => IO[B]): IO[B] = {
      FlatMap(fa, f)
    }
  }
}