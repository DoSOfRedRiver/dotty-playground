package io

import predef.StackSafeMonad
import io.runtime.Callback

enum IO[A] {
    case Pure(a: A)
    case Suspend(a: () => A)
    case Error(e: Exception)
    case Handle(onError: IO[A], cont: IO[A])
    case Map[A, A1](prev: IO[A1], f: A1 => A) extends IO[A]
    case FlatMap[A, A1](prev: IO[A1], f: A1 => IO[A]) extends IO[A]
    case Async[A](cont: Callback[A] => IO[Unit]) extends IO[A]

    def handleErrorWith(onError: IO[A]): IO[A] = Handle(onError, this)
}

trait Fiber[A] {
  def join: IO[A]
  def cancel: IO[Unit]
}

object IO {
  def suspend[A](a: => A) =
    Suspend(() => a)

  def asyncF[A](cont: Callback[A] => IO[Unit]): IO[A] =
    Async(cont)

  def async[A](cont: Callback[A] => Unit): IO[A] =
    asyncF[A](cb => suspend(cont(cb)))

  def start[A](ia: IO[A]): Fiber[A] = ???

  def raise[A](e: Exception): IO[A] =
    IO.suspend(throw e)

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