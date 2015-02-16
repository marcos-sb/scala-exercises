
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

}

//println(Stream(1,2,3).toList)
//println(Stream(1,2,3).take(2).toList)
//println(Stream(1,2,3,4).drop(2).toList)
println(Stream(1,2,3,4).takeWhile(_ < 3).toList)

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
