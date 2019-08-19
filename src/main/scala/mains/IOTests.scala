package mains

import io.runtime.FreeIORuntimeWithErrorHandling._
import predef._
import delegate predef._
import io._
import scala.io.StdIn.readLine


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

  val asyncTest: IO[Unit] = 
    for {
      _ <- printLn[IO]("Reading something asynchronously")
      _ <- IO.suspend(throw new RuntimeException("Boom!"))
      r <- IO.async[String] { clb =>
        import scala.util.{Success, Failure}

        apiCall.onComplete {
          case Success(value) => 
            clb(Right(value))
          case Failure(error: Exception) => 
            clb(Left(error))
        }
      }
      _ <- printLn(s"Got result from async: $r")
    } yield ()

  asyncTest.handleErrorWith(printLn("An error occured")).runUnsafe
}