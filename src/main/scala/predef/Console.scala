package predef

import io.IO

trait Console[F[_]] {
  def printLn(msg: String): F[Unit]
  def readLn: F[String]
}

inline def readLn[F[_]](given Console[F]): F[String] = summon[Console[F]].readLn
inline def printLn[F[_]](msg: String)(given Console[F]): F[Unit] = summon[Console[F]].printLn(msg)

given Console[IO] {
  def printLn(msg: String): IO[Unit] = IO.suspend(println(msg))
  def readLn: IO[String] = IO.suspend(scala.io.StdIn.readLine)
}