
import Stream._
sealed trait Stream[+A] {

  def headOption: Option[A] = this match {
    case Empty => None
    case Cons(h, t) => Some(h())
  }

  // EX1
  def toList: List[A] = this match {
    case Cons(h, t) => h() :: t().toList
    case Empty => Nil
  }

  // EX2
  def take(n: Int): Stream[A] = {
    def go(s: Stream[A], _n: Int): Stream[A] = s match {
      case _ if _n <= 0 => Empty
      case Cons(h, t) => cons(h(), go(t(), _n-1))
      case Empty => Empty
    }
    go(this, n)
  }

  def drop(n: Int): Stream[A] = this match {
    case Empty => Empty
    case c if n <= 0 => c
    case Cons(h, t) => t().drop(n-1)
  }

  // EX3
  def takeWhile(p: A => Boolean): Stream[A] = this match {
    case Cons(h, t) => if(p(h())) cons(h(), t().takeWhile(p))
                       else Empty
    case Empty => Empty
  }

  def exists(p: A => Boolean): Boolean = this match {
    case Cons(h, t) => p(h()) || t().exists(p)
    case _ => false
  }

  def foldRight[B](z: => B)(f: (A, => B) => B): B =
    this match {
      case Cons(h, t) => f(h(), t().foldRight(z)(f))
      case _ => z
    }

  def exists2(p: A => Boolean): Boolean =
    foldRight(false)((a, b) => p(a) || b)

  // EX4
  def forAll(p: A => Boolean): Boolean = this match {
    case Cons(h, t) => p(h()) && t().forAll(p)
    case _ => true
  }

  def forAll2(p: A => Boolean): Boolean =
    foldRight(true)((a, b) => p(a) && b)

  // EX5
  def takeWhile2(p: A => Boolean): Stream[A] =
    foldRight(empty[A])((a,b) => if (p(a)) cons(a, b) else empty)

  // EX6
  def headOption2: Option[A] =
    foldRight(None: Option[A])((h,_) => Some(h))

  // EX7
  def map[B](f: A => B): Stream[B] =
    foldRight(empty[B])((a,b) => cons(f(a), b))

  def filter(p: A => Boolean): Stream[A] =
    foldRight(empty[A])((a,b) =>
      if(p(a)) cons(a, b)
      else b)

  def append[B>:A](s: => Stream[B]): Stream[B] =
    foldRight(s)(cons(_,_))

  def flatMap[B](f: A => Stream[B]): Stream[B] =
    foldRight()
}

//println(Stream(1,2,3).toList)
//println(Stream(1,2,3).take(2).toList)
//println(Stream(1,2,3,4).drop(2).toList)
//println(Stream(1,2,3,4).takeWhile(_ < 3).toList)
//println(Stream(1,2,3,4).forAll(_ < 5))
//println(Stream(1,2,3,4).forAll2(_ < 5))
//println(Stream(1,2,3,4).takeWhile2(_ < 3).toList)
//println(Stream(1,2,3,4).headOption2)
//println(Empty.headOption2)
//println(Stream(1,2,3,4).map(_ + 1).toList)
//println(Stream(1,2,3,4).filter(_ % 2 == 0).toList)
//println(Stream(1,2,3,4).append(Stream(1,2,3)).toList)

case object Empty extends Stream[Nothing]
case class Cons[+A](h: () => A, t: () => Stream[A]) extends Stream[A]

object Stream {

  def cons[A](hd: => A, tl: => Stream[A]): Stream[A] = {
    lazy val head = hd
    lazy val tail = tl
    Cons(() => head, () => tail)
  }

  def empty[A]: Stream[A] = Empty

  def apply[A](as: A*): Stream[A] =
    if(as.isEmpty) empty else cons(as.head, apply(as.tail: _*))

}
