package io.runtime

import io.IO
import io.runtime.BlockingCallback
import io.runtime.StackFrame

class IORuntime(given Scheduler) extends Runtime[IO] {
  import scala.collection.mutable.ArrayStack

  type Bind = Any => IO[Any]
  type CallStack = ArrayStack[Bind]

  var sch = summon[Scheduler]

  def[A] (fa: IO[A]) runUnsafe: A = {
    val clb = new BlockingCallback[A]

    //TODO this is not thread-safe
    val binds: CallStack = ArrayStack()

    summon[Scheduler].execute { () =>
      clb(Right(runLoop(fa, binds)))
    }

    clb.value
  }

  def runLoop[A](fa: IO[A], binds: CallStack): A = {
    var current = fa.asInstanceOf[IO[Any]]
    var hasResult = false
    var result: Any = null


    inline def setResult(r: Any): Unit = {
      result = r
      hasResult = true
    }

    while(true) {
      current match {
        case IO.Pure(a) =>
          setResult(a)

        case IO.Suspend(thunk) =>
          try {
            setResult(thunk())
          } catch {
            case e: Exception =>
              current = IO.Error(e)
          }

        case IO.Error(e) =>
          findErrorHandler(binds) match {
            case null =>
              throw e
            case frame: ErrorHandler[Any, IO[Any]] =>
              current = frame.recover(e)
          }

        case IO.Map(fa, f) =>
          current = fa.asInstanceOf[IO[Any]]
          val bind = IO.Pure.apply.compose(f).asInstanceOf[Bind]
          binds.push(bind)

        case IO.FlatMap(fa, f) =>
          current = fa.asInstanceOf[IO[Any]]
          binds.push(f.asInstanceOf[Bind])

        case IO.Shift(sc) =>
          sch = sc
          current = binds.pop()(().asInstanceOf[Any])

        case IO.Fork(cont) =>
          setResult(forkCont(cont, sch))

        case async: IO.Async[_] =>
          setResult(executeAsync(async, binds, sch))
      }

      if (hasResult) {
        if (binds.isEmpty) {
          return result.asInstanceOf[A]
        } else {
          val bindNext = binds.pop()
          try {
            current = bindNext(result)
          } catch {
            case e: Exception =>
              current = IO.Error(e)
          }
          hasResult = false
          result = null
        }
      }
    }

    throw new IllegalStateException("out of loop")
  }

  def executeAsync[A](async: IO.Async[A], binds: CallStack, sc: Scheduler): A = {
    val clb = new BlockingCallback[A]

    //TODO does these "binds" actually needed?
    sc.execute(() => clb(Right(async1(async, binds))))

    clb.value
  }

  inline def async1[A](async: IO.Async[A], binds: CallStack): A = {
    val clb = new BlockingCallback[A]
    val current = async.cont(clb) >> IO.suspend(clb.value)
    runLoop(current, binds)
  }

  def forkCont[A](cont: IO[A], sc: Scheduler): Fiber[A] = 
    new Fiber[A] {
      val bcb = new BlockingCallback[A]()
      
      sc.execute { () => 
        try {
          val r = runLoop(cont, ArrayStack())
          bcb(Right(r))
        } catch {
          case e: Exception =>
            bcb(Left(e))
        }
      }

      def join: IO[A] = IO.async { cb =>
        cb(bcb.valueEither)
      }
      def cancel: IO[Unit] = ???
    }

  def findErrorHandler(cs: CallStack): ErrorHandler[Any, IO[Any]] = {
    while {
      val bind = cs.pop()
      if (bind.isInstanceOf[ErrorHandler[_, _]]) {
        return bind.asInstanceOf[ErrorHandler[Any, IO[Any]]]
      }

      cs.nonEmpty
    } do ()

    null
  }
}