package mains

object FreeTests extends App {
  import delegate free._
  import predef.Monad
  import free.Free
  import io.IO
  import io.runtime.FreeIORuntime._
  import scala.io.StdIn.readLine

  enum ConsoleA[A] {
    case ReadLine extends ConsoleA[String]
    case PrintLine(msg: String) extends ConsoleA[Unit]
  }

  type Console[A] = Free[ConsoleA, A]

  def readLnF: Console[String] = {
    Free.liftF(ConsoleA.ReadLine)
  }


  def printLnF(msg: String): Console[Unit] = {
    Free.liftF(ConsoleA.PrintLine(msg))
  }

  val freeProgram = 
    for {
      _     <- printLnF("Welcome to free program")
      _     <- printLnF("Type your name: ")
      name  <- readLnF
      _     <- printLnF(s"Hello, $name")
    } yield ()

  println("Starting execution..")
  freeProgram.foldMap[IO]([X] => (con: ConsoleA[X]) => con match {
    case ConsoleA.ReadLine =>
      IO.suspend(readLine).asInstanceOf[IO[X]]
    case ConsoleA.PrintLine(msg) =>
      IO.suspend(println(msg)).asInstanceOf[IO[X]]
  }).runUnsafe

  println("End execution")
}