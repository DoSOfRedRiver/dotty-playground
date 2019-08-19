package predef

delegate [X: Monoid] for StackSafeMonad[[A] =>> (X, A)] {
  def (fa: (X, A)) flatMap[A, B] (f: A => (X, B)): (X, B) = {
    val xb = f(fa._2)
    val x = fa._1 combine xb._1
    (x, xb._2)
  }

  def (a: A) pure[A]: (X, A) = (the[Monoid[X]].empty, a)
}


delegate for Monad[Function0] {
  def (fa: Function0[A]) flatMap[A,B] (f: A => Function0[B]): Function0[B] = {
    f(fa())
  }

  def (a: A) pure[A]: Function0[A] = () => a
}