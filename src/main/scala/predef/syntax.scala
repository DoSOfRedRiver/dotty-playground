package predef

def (b: B) right[A, B]: Either[A, B] = Right(b)
def (a: A) left[A, B]: Either[A, B] = Left(a)