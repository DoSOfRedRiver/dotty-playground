package io.runtime

trait StackFrame[-A, +R] extends (A => R)

class ErrorHandler[-A, +R](fa: A => R, fe: Exception => R) extends StackFrame[A, R] {
  def apply(a: A): R = fa(a)
  def recover(e: Exception): R = fe(e)
}