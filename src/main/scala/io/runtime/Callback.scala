package io.runtime

import java.util.concurrent.locks.AbstractQueuedSynchronizer

trait Callback[A] extends (Either[Exception, A] => Unit) {
  def onSuccess(value: A): Unit

  def onError(e: Exception): Unit

  def apply(result: Either[Exception, A]): Unit =
    result match {
      case Right(a) => onSuccess(a)
      case Left(e) => onError(e)
    }
}

final class OneShotLatch extends AbstractQueuedSynchronizer {
  override protected def tryAcquireShared(ignored: Int): Int =
    if (getState != 0) 1 else -1

  override protected def tryReleaseShared(ignore: Int): Boolean = {
    setState(1)
    true
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

  def onSuccess(value: A): Unit = {
    success = value
    latch.releaseShared(1)
  }

  def onError(ex: Exception): Unit = {
    error = ex
    latch.releaseShared(1)
  }
}