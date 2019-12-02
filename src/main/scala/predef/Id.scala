package predef.id

import predef.Monad

type Id[A] = A

given IdMonad: Monad[Id] {
  def[A] (a: A) pure: Id[A] = a
  def[A,B] (a: Id[A]) flatMap (f: A => Id[B]): Id[B] = f(a)
}