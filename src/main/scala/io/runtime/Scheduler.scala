package io.runtime

import scala.concurrent.ExecutionContext

trait Scheduler {
  def execute(r: Runnable): Unit
}

object Scheduler {
  def fromEC(ec: ExecutionContext) = new Scheduler {
    def execute(r: Runnable) = ec.execute(r)
  }

  def default = new Scheduler {
    def execute(r: Runnable) = r.run
  }
}