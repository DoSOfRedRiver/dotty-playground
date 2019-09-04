package mains

import scala.io.StdIn.readLine
import io.runtime.IORuntime
import io.runtime.Scheduler
import io._
import predef._
import delegate predef._


import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future

object IOTests extends App {

  val helloWorld = 
    for {
      name  <- IO.suspend(readLine("Type your name: "))
      _     <- IO.suspend(println(s"Hello, $name"))
    } yield ()

  val ioSleep: IO[Unit] = IO.suspend(Thread.sleep(100))

  var c = 0

  val incCounter: IO[Unit] = {
    IO.suspend(c += 1)
  }

  val printIfModulo: IO[Unit] = {
    IO.suspend {
      if (c % 10000 == 0) println(s"New count ${c}")
    }
  }

  val simplePrintCycle: IO[Unit] = 
    incCounter >> printIfModulo >> simplePrintCycle

  val apiCall = Future {
    Thread.sleep(5000)
    "I come from the Future!"
  }

  val subAsync = 
    for {
      + <- printLn("subAsync")
      _ <- printLn(Thread.currentThread.getName)
      r <- IO.async[String] { clb =>
        import scala.util.{Success, Failure}

        apiCall.onComplete {
          case Success(value) => 
            clb(Right(value))
          case Failure(error: Exception) => 
            clb(Left(error))
        }
      }

      _ <- printLn("After async")
      _ <- printLn(Thread.currentThread.getName)

      //_ <- IO.suspend(throw new RuntimeException("Boom!"))
    } yield ()

  
  val global = scala.concurrent.ExecutionContext.Implicits.global
  val globalSc: Scheduler = Scheduler.fromEC(global)
  implicit val sc: Scheduler = Scheduler.default
  implicit val runtime: IORuntime = new IORuntime
  
  val asyncTest: IO[String] = 
    for {
      _ <- printLn[IO]("Reading something asynchronously")
      _ <- printLn(Thread.currentThread.getName)
      _ <- IO.shift(globalSc)
      r <- subAsync
      _ <- printLn("Third")
      _ <- printLn(Thread.currentThread.getName)
    } yield ""

  def printTimes(msg: String, times: Int): IO[Unit] = {
    if (times > 0)
      printLn(s"$times: $msg") >> IO.suspend(Thread.sleep(1000)) >> printTimes(msg, times - 1)
    else
      IO.unit
  }

  val forkTest: IO[Unit] =
    for {
      _   <- IO.shift(globalSc)
      f1  <- printTimes("First", 10).fork
      _   <- printLn("Fire first")
      f2  <- printTimes("Second", 10).fork
      _   <- printLn("Fire second")
      f3  <- printTimes("Third", 10).fork
      _   <- printLn("Fire third")
      _   <- f1.join
      _   <- f2.join
      _   <- f3.join
      _   <- printLn("Done")
    } yield ()

  forkTest
    .runUnsafe
}