package io.runtime

import scala.concurrent.ExecutionContext

trait Callback[A] extends (Either[Exception, A] => Unit) {
  def onSuccess(value: A): Unit

  def onError(e: Exception): Unit

  def apply(result: Either[Exception, A]): Unit =
    result match {
      case Right(a) => onSuccess(a)
      case Left(e) => onError(e)
    }
}

class BlockingCallback[A] extends Callback[A] {
  val latch = new OneShotLatch

  private var success: A = _
  private var error: Exception = _

  import scala.concurrent.blocking

  def value: A = {
    blocking(latch.acquireSharedInterruptibly(1)) 
    
    error match {
      case null => success
      case e => throw e
    }
  }

  def valueEither: Either[Exception, A] = {
    blocking(latch.acquireSharedInterruptibly(1))

    error match {
      case null => 
        Right(success)
      case e => 
        Left(e)
    }
  }

  def onSuccess(value: A): Unit = {
    success = value
    latch.releaseShared(1)
  }

  def onError(ex: Exception): Unit = {
    error = ex
    latch.releaseShared(1)
  }
}