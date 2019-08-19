package predef

final case class Const[A, B](const: A) {
  def retag[C]: Const[A, C] = this.asInstanceOf[Const[A, C]]

  def combine(that: Const[A, B]) given Semigroup[A]: Const[A, B] =
    Const(const combine that.const)
}

delegate ConstApplicative[C: Monoid] for Applicative[[X] =>> Const[C, X]] {
  def (x: X) pure[X]: Const[C, X] = Const(the[Monoid[C]].empty)

  def (fa: Const[C, A]) ap[A,B] (fab: Const[C, A => B]): Const[C, B] = {
    fab.retag[B].combine(fa.retag[B])
  }

  def (fa: Const[C, A]) map[A, B](f: A => B): Const[C, B] = fa.retag
}