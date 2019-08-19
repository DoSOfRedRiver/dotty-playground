package predef.id

import predef.Monad

type Id[A] = A

delegate IdMonad for Monad[Id] {
  def (a: A) pure[A]: Id[A] = a
  def (a: Id[A]) flatMap[A,B](f: A => Id[B]): Id[B] = f(a)
}