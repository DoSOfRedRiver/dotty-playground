package predef

given [X: Monoid]: StackSafeMonad[[A] =>> (X, A)] {
  def[A, B] (fa: (X, A)) flatMap (f: A => (X, B)): (X, B) = {
    val xb = f(fa._2)
    val x = fa._1 combine xb._1
    (x, xb._2)
  }

  def[A] (a: A) pure: (X, A) = (summon[Monoid[X]].empty, a)
}


given Monad[Function0] {
  def[A,B] (fa: Function0[A]) flatMap (f: A => Function0[B]): Function0[B] = {
    f(fa())
  }

  def[A] (a: A) pure: Function0[A] = () => a
}

given Monad[List] {
  def[A,B] (fa: List[A]) flatMap (f: A => List[B]): List[B] = {
    fa.flatMap(f)
  }

  def[A] (a: A) pure: List[A] = List(a)
}

given Monad[Option] {
  def[A,B] (fa: Option[A]) flatMap (f: A => Option[B]): Option[B] = {
    fa.flatMap(f)
  }

  def[A] (a: A) pure: Option[A] = Some(a)
}