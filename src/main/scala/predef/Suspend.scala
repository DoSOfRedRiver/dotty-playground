package predef

trait Suspend[F[_]] {
    def[A] (a: => A) suspend: F[A]
}