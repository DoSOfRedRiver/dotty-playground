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

delegate for Monad[List] {
  def (fa: List[A]) flatMap[A,B] (f: A => List[B]): List[B] = {
    fa.flatMap(f)
  }

  def (a: A) pure[A]: List[A] = List(a)
}

delegate for Monad[Option] {
  def (fa: Option[A]) flatMap[A,B] (f: A => Option[B]): Option[B] = {
    fa.flatMap(f)
  }

  def (a: A) pure[A]: Option[A] = Some(a)
}