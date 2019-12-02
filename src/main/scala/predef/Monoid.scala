package predef

trait Semigroup[A] {
  def (l: A) combine (r: A): A
}

trait Monoid[A] extends Semigroup[A] {
  def empty: A
}

given IntMonoid: Monoid[Int] {
  def empty = 0
  def (l: Int) combine (r: Int) = l + r
}

given StringMonoid: Monoid[String] {
  def empty = ""
  def (l: String) combine (r: String) = l + r
}

given ListMonoid[A]: Monoid[List[A]] {
  def empty = List.empty
  def (l: List[A]) combine (r: List[A]) = l ++ r
}