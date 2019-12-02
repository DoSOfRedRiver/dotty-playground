package mains

import free.Free
import free.Free._
import predef.Const
import predef.TailRec
import io.given
import free.given
import predef.given
import java.util.concurrent.atomic.AtomicInteger

object Main extends App {

  type Pair[A] = (A, A)
  type BinTree[A] = Free[Pair, A]

  val tree = Suspend[Pair, BinTree[Int]]((Suspend((1, 2)), Pure(3))).flatten
  val res = tree.foldMap([X] => (pair: Pair[X]) => List(pair._1, pair._2))
  println(res)

  final case class Fix[F[_]](unfix: F[Fix[F]])

  type Id[A] = A
  type ListF[A] = Free[[X] =>> (A, X), Unit]

  val empty: ListF[Int] = Pure(())
  val cons: ListF[Int] = Suspend((1, Suspend(2, Suspend(3, empty)))).flatten.flatten.flatten

  val re = cons.foldMap([X] => (cl: (Int, X)) => cl match {
    case (i, x) =>
      (List(i), x)
  })

  println(s"Re is $re")
}