package mains

object SchemesTests extends App {
  import schemes._
  import schemes.given

  def genN[F[_]: Ana](n: Int): F[Int] = {
    n.unfold(_ == 0,  b => (b, b-1))
  }

  def sum[F[_]: Cata](fi: F[Int]): Int = {
    fi.foldr(0)(_ + _)
  }

  def wtf: Int => Int = {
    genN[List] andThen sum[List]
  }

  def facHylo: Int => Int = 
    hylo[B = Int](_) (
      1,
      (a, c) => a * c,
      a => (a, a - 1),
      _ == 0
    )

  println(s"facHylo 5 = ${facHylo(5)}")

  def mapCata[A,B](la: List[A], f: A => B): List[B] = {
    la.foldr(List.empty) { case (a, bs) => f(a) :: bs }
  }

  val l1 = List(1,2,3)
  println(s"MapCata \n\t$l1 \n\t${mapCata(l1, _ + 1)}")

  def mapAna[A, B](la: List[A], f: A => B): List[B] = {
    la.unfold(
      _.isEmpty,
      (_: @unchecked) match { case (a :: as ) =>
        (f(a), as)
      }
    )
  }

  println(s"MapAna\n\t$l1\n\t${mapAna(l1, _ + 1)}")

  def[A, B] (l: List[A]) zipAna (r: List[B]): List[(A,B)] = {
    //compiler hangs if inline
    val tup: (List[A], List[B]) = (l, r)
    (tup).unfold (
      _ match { case (al, bl) => al.isEmpty || bl.isEmpty }, 
      (_: @unchecked) match { case (a :: as, b :: bs) => ((a,b), (as, bs)) }
    )
  }

  println(wtf(5) == 15)
  println(List(1,2,3) zipAna List(4,5,6,7))


  println(s"Para ${l1.para(0, (a,b) => s"$a$b".toInt)}")

  enum Num {
    case Zero
    case Succ(prev: Num)

    def toInt: Int = this match {
      case Num.Zero => 0
      case Num.Succ(prev) => 
        prev.toInt + 1
    }

    def * (num: Num): Num = {
      if (this == Zero || num == Zero) Zero
      else (this.toInt * num.toInt).toNum
    }
  }

  object Num {
    val One = Succ(Zero)
  }

  def (n: Int) toNum: Num = {
    if (n == 0) Num.Zero
    else Num.Succ((n - 1).toNum)
  }

  def[B] (num: Num) paraNum (z: B)(f: (Num, B) => B): B = num match {
    case Num.Zero => z
    case Num.Succ(prev) =>
      f(prev, prev.paraNum(z)(f))
  }


  def paraFac(n: Num): Num = {
    println(s"para: $n ${Num.One}")
    n.paraNum(Num.One)((n, m) => Num.Succ(n) * m)
  }

  println(s"paraFac(5) ${paraFac(5.toNum).toInt}")
}