package predef

trait Raise[F[_]] {
    def[A] (e: Exception) raise: F[A]
}