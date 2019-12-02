package schemes

trait Cata[F[_]] {
  def[A, B] (fa: F[A]) foldr (z: B)(f: (A, B) => B): B
}

trait Ana[F[_]] {
  def[A, B] (b: B) unfold (p: B => Boolean, g: B => (A,B)): F[A]
}

trait Para[F[_]] {
  def[A, B] (fa: F[A]) para (z: B, f: (A, B) => B): B
}

def hylo[A, B, C](a: A)(z: C, f: (B, C) => C, g: A => (B, A), p: A => Boolean): C = {
  if (p(a)) z
  else {
    val (b, a1) = g(a)
    f(b, hylo(a1)(z, f, g, p))
  }
}

given Cata[List] {
  def[A, B] (fa: List[A]) foldr (z: B)(f: (A, B) => B): B = {
    fa match {
      case Nil => z
      case a :: as =>
        f(a, as.foldr(z)(f))
    }
  }
}

given Ana[List] {
  def[A, B] (b: B) unfold (p: B => Boolean, g: B => (A,B)): List[A] = {
    if (p(b)) Nil
    else {
      val (a, b1) = g(b)
      a :: b1.unfold(p, g)
    }
  }
}

given Para[List] {
  def[A, B] (fa: List[A]) para (z: B, f: (A, B) => B): B = {
    fa match {
      case Nil => z
      case a :: as =>
        f(a, as.para(z, f))
    }
  }
}