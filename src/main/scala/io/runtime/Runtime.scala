package io.runtime

import io.IO
import free._
import free.Free.defer
import predef._
import io.runtime.BlockingCallback
import scala.concurrent.ExecutionContext

import free.given
import io.given
import predef.given

trait Runtime[F[_]] {
  def[A] (fa: F[A]) runUnsafe: A
}

/*object TrampolineIORuntime extends Runtime[IO] {
  import runtime.trampoline.{suspend, result}
  import runtime.trampoline.Trampoline

  def (fa: IO[A]) runUnsafe[A]: A = {
    def loop[A](fa: IO[A]): Trampoline[A] = {
      fa match {
        case IO.Pure(a) =>
          result(a)
        case IO.Suspend(cont) =>
          suspend(result(cont()))
        case IO.Map(prev, f) =>
          loop(prev) map f
        case IO.FlatMap(prev, f) =>
          suspend(loop(prev)).flatMap(a => suspend(loop(f(a))))
      }
    }

    loop(fa).run
  }
}*/

object FreeIORuntime extends Runtime[IO] {
  def[A] (fa: IO[A]) runUnsafe: A = {
    loop1(fa).eval(_())
  }

  def loop1[A](fa: IO[A]): Free[Function0, A] = {
    fa match {
      case IO.Pure(a) =>
        Free.Pure(a)
      case IO.Suspend(cont) =>
        Free.Suspend(cont)
      case IO.Error(e) =>
        throw e
      case IO.Map(prev, f) =>
        Free.FlatMap(loop1(prev), x => Free.Pure(f(x)))
      case IO.FlatMap(prev, f) =>
        Free.FlatMap(loop1(prev), x => loop1(f(x)))
      case IO.Async(cont) =>
        val clb = new BlockingCallback[A]
        loop1(
          cont.asInstanceOf[BlockingCallback[A] => IO[Unit]](clb) >> IO.suspend(clb.value)
        )
    }
  }
}

object FreeIORuntimeWithErrorHandling extends Runtime[IO] {
  type Run[A] = EitherT[Function0, Exception, A]

  def[A] (fa: IO[A]) runUnsafe: A = {
    eval(loop(fa)) match {
      case IO.Error(e) =>
        throw e
      case io =>
        FreeIORuntime.loop1(io).eval(_())
    }
  }


  def unwrap[A](x: Run[Free[Run, A]]): Free[Run, A] = {
    x.value() match {
      case Left(e) =>
        throw e
      case Right(v) =>
        v
    }
  }

  import scala.annotation.tailrec
  def eval[A](comp: Free[Run, IO[A]]): IO[A] = {
    @tailrec
    def loop(free: Free[Run, IO[A]]): IO[A] = {
      val step = 
        try {
          free.resume  
        } catch {
          case e: Exception =>
            Right(IO.Error(e))
        }

      step match {
        case Right(a) => a
        case Left(step) => 
          loop(unwrap(step))
      }
    }

    loop(comp)
  }

  def suspendFreeRun[A](ia: IO[A]): Free[Run, IO[A]] = Free.Suspend(EitherT(() => Right(ia)))

  def loop[A](fa: IO[A]): Free[Run, IO[A]] = {
    fa match {
      case IO.Pure(a) =>
        Free.Pure(IO.Pure(a))
      case IO.Suspend(cont) =>
        suspendFreeRun(IO.Suspend(cont))
      case IO.Error(e) =>
        Free.Suspend(EitherT(() => Left(e)))
      case IO.Map(prev, f) =>
        Free.FlatMap(loop(prev), {
            case IO.Pure(a) =>
              loop(IO.Pure(f(a)))
            case IO.Error(e) =>
              loop(IO.Error(e))
            case IO.Suspend(c) =>
              loop(IO.Suspend(c.asInstanceOf[Function0[A]]))
            case _ => ???
          }
        )
      case IO.FlatMap(prev, f) =>
        Free.FlatMap(loop(prev), {
          case IO.Pure(a) =>
            loop(f(a))
          case IO.Suspend(c) =>
            loop(IO.Suspend(c.asInstanceOf[Function0[A]]))
          case IO.Error(e) =>
            loop(IO.Error(e))
          case _ => ???
        })
      case IO.Async(cont) =>
        val clb = new BlockingCallback[A]
        loop(
          cont.asInstanceOf[BlockingCallback[A] => IO[Unit]](clb) >> IO.suspend(clb.value)
        )
    }
  }
}