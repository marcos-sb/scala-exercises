
// EX6
sealed trait Either[+E, +A] {
  def map[B](f: A => B): Either[E, B] = this match {
    case Left(e) => Left(e)
    case Right(v) => Right(f(v))
  }

  def flatMap[EE >: E, B](f: A => Either[EE, B]): Either[EE, B] = this match {
    case Left(e) => Left(e)
    case Right(v) => f(v)
  }

  def orElse[EE >: E, B >: A](b: => Either[EE, B]): Either[EE, B] = this match {
    case Left(_) => b
    case Right(v) => Right(v)
  }

  def map2[EE >: E, B, C](b: Either[EE, B])(f: (A,B) => C): Either[EE, C] =
    this flatMap (aa => b map (bb => f(aa,bb)))

}
case class Left[+E](value: E) extends Either[E, Nothing]
case class Right[+A](value: A) extends Either[Nothing, A]


def mean(xs: IndexedSeq[Double]): Either[String, Double] =
  if (xs.isEmpty)
    Left("mean of empty List!")
  else
    Right(xs.sum / xs.length)


def safeDiv(x: Int, y: Int): Either[Exception, Int] =
  try Right(x / y)
  catch { case e: Exception => Left(e) }


def Try[A](a: => A): Either[Exception, A] =
  try Right(a)
  catch { case e: Exception => Left(e) }
