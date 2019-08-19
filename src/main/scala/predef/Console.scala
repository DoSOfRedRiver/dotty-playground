package predef

import io.IO

trait Console[F[_]] {
  def printLn(msg: String): F[Unit]
  def readLn: F[String]
}

inline def readLn[F[_]] given Console[F]: F[String] = the[Console[F]].readLn
inline def printLn[F[_]](msg: String) given Console[F]: F[Unit] = the[Console[F]].printLn(msg)

delegate for Console[IO] {
  def printLn(msg: String): IO[Unit] = IO.suspend(println(msg))
  def readLn: IO[String] = IO.suspend(scala.io.StdIn.readLine)
}