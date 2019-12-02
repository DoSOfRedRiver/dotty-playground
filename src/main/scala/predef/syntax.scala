package predef

def[A, B] (b: B) right: Either[A, B] = Right(b)
def[A, B] (a: A) left: Either[A, B] = Left(a)