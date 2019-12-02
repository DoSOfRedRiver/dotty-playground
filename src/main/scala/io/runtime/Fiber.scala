package io.runtime

import io.IO
import predef._
import io.IO.given
import predef.given

trait Fiber[+A] {
  def join: IO[A]
  def cancel: IO[Unit]
}

object Fiber {
  def apply[A](ia: IO[A], canc: IO[Unit]): Fiber[A] = {
    new Fiber[A] {
      def join: IO[A] = ia
      def cancel: IO[Unit] = canc
    }
  }
}

def[A] (ia: IO[A]) fork: IO[Fiber[A]] = {
  ia match {
    case IO.Pure | IO.Error =>
      IO.Pure(Fiber(ia, IO.unit))

    case _ =>
      IO.asyncF[Fiber[A]](cb => forkAsync(ia) map (cb compose right))
  }
}

def forkAsync[A](ia: IO[A]): IO[Fiber[A]] = {
  new Fiber[A] {
    def join: IO[A] = ???
    def cancel: IO[Unit] = ???
  }
  ???
}