package schemes

trait Cata[F[_]] {
  def (fa: F[A]) foldr[A, B] (z: B)(f: (A, B) => B): B
}

trait Ana[F[_]] {
  def (b: B) unfold[A, B](p: B => Boolean, g: B => (A,B)): F[A]
}

trait Para[F[_]] {
  def (fa: F[A]) para[A, B](z: B, f: (A, B) => B): B
}

def hylo[A, B, C](a: A)(z: C, f: (B, C) => C, g: A => (B, A), p: A => Boolean): C = {
  if (p(a)) z
  else {
    val (b, a1) = g(a)
    f(b, hylo(a1)(z, f, g, p))
  }
}

delegate for Cata[List] {
  def (fa: List[A]) foldr[A, B] (z: B)(f: (A, B) => B): B = {
    fa match {
      case Nil => z
      case a :: as =>
        f(a, as.foldr(z)(f))
    }
  }
}

delegate for Ana[List] {
  def (b: B) unfold[A, B](p: B => Boolean, g: B => (A,B)): List[A] = {
    if (p(b)) Nil
    else {
      val (a, b1) = g(b)
      a :: b1.unfold(p, g)
    }
  }
}

delegate for Para[List] {
  def (fa: List[A]) para[A, B](z: B, f: (A, B) => B): B = {
    fa match {
      case Nil => z
      case a :: as =>
        f(a, as.para(z, f))
    }
  }
}