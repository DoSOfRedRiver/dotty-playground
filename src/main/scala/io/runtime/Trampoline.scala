package runtime

import scala.annotation.tailrec

enum Trampoline[A] {
  case Suspend(cont: () => Trampoline[A])
  case Result(a: A)
  case FlatMap[A, B](prev: Trampoline[A], f: A => Trampoline[B]) extends Trampoline[B]

  def run: A = runTrampoline(this)

  def flatMap[B](f: A => Trampoline[B]): Trampoline[B] = {
    this match {
      case FlatMap(prev, g) =>
        FlatMap(prev, g(_) flatMap f)
      case prev => 
          FlatMap(prev, f)
    }
  }

  def map[B](f: A => B): Trampoline[B] = flatMap(f andThen Result.apply)
}

@tailrec
def runTrampoline[A](t: Trampoline[A]): A = {
  import Trampoline._
  t match {
    case Result(a) =>
      a
    case Suspend(thunk) =>
      runTrampoline(thunk())
    case FlatMap(prev, f) =>
      val cont = prev match {
        case Result(a) =>
          f(a)
        case Suspend(thunk) =>
          Suspend(() => thunk().flatMap(f))
        case FlatMap(prev1, f1) =>
          prev1.flatMap(f1(_).flatMap(f))
      }
      runTrampoline(cont)
  }
}

def suspend[A](a: => Trampoline[A]) = Trampoline.Suspend(() => a)
def result[A](a: A) = Trampoline.Result(a)