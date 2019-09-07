package mains

import predef._
import delegate predef._
import io.IO
import io.runtime.Scheduler
import io.runtime.IORuntime

object Test {
  type W[A, X] = A & X

  type EffI[F[_]] = Monad[F]

  type Next[X, Y] = [A <: X | Y] =>> A match {
    case X => Y
    case Y => X
  }

  type This[X, Y] = [A <: X | Y] =>> A match {
    case X => X
    case Y => Y
  }

  trait Tag[A] {
    def name: String
  }

  given StringTag as Tag[String] {
    def name: String = "String"    
  }

  given IntTag as Tag[Int] {
    def name: String = "Int"
  }

  trait Iso[A, B] {
    def from(b: B): A
    def to(a: A): B
  }

  given [A, B, C <: A | B] as Iso[C, This[A, B][C]] {
    def from(next: This[A, B][C]): C = {
      next.asInstanceOf[C]
    }

    def to(c: C): This[A, B][C] = {
      c.asInstanceOf[This[A, B][C]]
    }
  }

  trait Jok[A, B] {
    type Out

    def apply[C <: A | B](ab: C)(implicit jok: Tag[Next[A, B][C]]): Out
  }

  class TestJok[A, B] {
    //def testJok[C <: A | B](ab: C)(implicit tag: Tag[Next[A, B][C]]): String = tag.name

    def next[C <: A | B](d: C)(implicit iso: Iso[C, This[A, B][C]]): Next[A, B][C] => (Next[A, B][C], This[A, B][C]) = {
      next =>
        (next, iso.to(d))
    }
  }

  def cr[A](a: A): A = ???

  inline def composite[A, B, C <: A | B](c: C): Next[A, B][C] => A & B = {
    next =>
      c match {
        case a: A =>
          /*(new {
            //BUG
            export a._
            export next._
          }: A & B)*/
          ???
        case b: B =>
          ???
      }
  }
}

opaque type Eff[F[_]] = Monad[F] with Console[F]

implicit def deriveCap[F[_]] given (m: Monad[F], c: Console[F]): Eff[F] = new Monad[F] with Console[F] {
  export m._
  export c._
}

trait Doo {
  def a: Int
}

trait Foo {
  def b: String
}

val doo: Doo = new Doo { val a = 4 }

val fooDoo: Doo with Foo = new Foo with Doo {
  export doo._
  val b = "String"
}

def doo[F[_]: Eff]: F[Unit] = {
  for {
    _ <- printLn[F]("Hello")
    _ <- printLn("World")
  } yield ()
}


object DeriveCapabilities extends App {
  implicit val sc: Scheduler = Scheduler.default
  implicit val runtime: IORuntime = new IORuntime

  import Test._
  //doo[IO].runUnsafe
  val f = new TestJok[Int, String].next("sd")
  println(f(4))
  
}