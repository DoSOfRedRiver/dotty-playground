package predef

final case class Const[A, B](const: A) {
  def retag[C]: Const[A, C] = this.asInstanceOf[Const[A, C]]

  def combine(that: Const[A, B])(given Semigroup[A]): Const[A, B] =
    Const(const combine that.const)
}

given ConstApplicative[C: Monoid]: Applicative[[X] =>> Const[C, X]] {
  def[X] (x: X) pure: Const[C, X] = Const(summon[Monoid[C]].empty)

  def[A,B] (fa: Const[C, A]) ap (fab: Const[C, A => B]): Const[C, B] = {
    fab.retag[B].combine(fa.retag[B])
  }

  def[A, B] (fa: Const[C, A]) map (f: A => B): Const[C, B] = fa.retag
}