package mains

import stream._

import predef.{Monad, Console, StackSafeMonad, Functor, Raise, printLn, readLn}

import io.runtime.IORuntime
import io.runtime.Scheduler
import io._

import stream.{given Monad[?]}
import predef.{given Console[?]}
import io.IO.{given StackSafeMonad[?]}

object StreamsTests extends App {
  type InvIO[A] = IO[A]

  //implicit val invIoFunctor: Functor[InvIO] = the[Functor[IO]]
  //println(s"Functor: ${the[Functor[IO]]}")

  implicit val invIoFunctor1: Functor[InvIO] = summon[StackSafeMonad[IO]].asInstanceOf[Functor[InvIO]]
  implicit val invIoMonad: Monad[InvIO] = invIoFunctor1.asInstanceOf[Monad[InvIO]]
  implicit val raiseInvIo: Raise[InvIO] = summon[Raise[IO]].asInstanceOf[Raise[InvIO]]

  val l: IO[List[Int]] = Stream.from[IO](1, 1)
    .map(_ + 1)
    .take(10)
    .toList
    
  val p = for {
    list  <- l
    _     <- printLn(s"Result is $list")
    } yield ()
    
  val reads = Stream.eval(readLn)
    .evalMap { str => 
      IO.suspend(str.toInt)
    }
    .map(x => x * x)
    .map(_.toString)
    .evalTap(x => printLn(x))
    .repeat

  val loop = Stream.eval(printLn("loop"))
      .evalTap(_ => IO.suspend(Thread.sleep(100)))
      .repeat

  val global = scala.concurrent.ExecutionContext.Implicits.global
  val globalSc: Scheduler = Scheduler.fromEC(global)
  implicit val sc: Scheduler = Scheduler.default
  implicit val runtime: IORuntime = new IORuntime

  println("Before")
  p.runUnsafe
}