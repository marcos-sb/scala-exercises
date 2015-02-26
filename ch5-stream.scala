
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
    foldRight(empty[B])((a,b) => f(a) append b)

  def find(p: A => Boolean): Option[A] =
    filter(p).headOption

  // EX13
  def map2[B](f: A => B): Stream[B] =
    unfold(this) {
      case Cons(h, t) => Some((f(h()), t()))
      case Empty => None
    }

  def take2(n: Int): Stream[A] =
    unfold((this,n)) {
      case (Cons(h, _), 1) => Some((h(), (empty[A], 0)))
      case (Cons(h, t), n) if n > 1 => Some((h(), (t(), n-1)))
      case _ => None
    }

  def takeWhile3(p: A => Boolean): Stream[A] =
    unfold(this) {
      case Cons(h, t) => if(p(h())) Some((h(), t()))
                         else None
      case _ => None
    }

  def zipWith[B,C](s2: Stream[B])(f: (A,B) => C): Stream[C] =
    unfold((this, s2)) {
      case (Empty, _) => None
      case (_, Empty) => None
      case (Cons(h1, t1), Cons(h2,t2)) => Some((f(h1(),h2()), (t1(),t2())))
    }

  def zipAll[B](s2: Stream[B]): Stream[(Option[A],Option[B])] =
    unfold((this, s2)) {
      case (Empty, Empty) => None
      case (Empty, Cons(h2,t2)) => Some(((None,Some(h2())),(empty[A], t2())))
      case (Cons(h1,t1), Empty) => Some(((Some(h1()), None),(t1(), empty[B])))
      case (Cons(h1,t1), Cons(h2,t2)) =>
        Some(((Some(h1()),Some(h2())),(t1(),t2())))
    }

  // EX14
  def startsWith[A](s: Stream[A]): Boolean =
    zipAll(s).takeWhile(!_._2.isEmpty) forAll {
      case (h,h2) => h == h2
    }

  // EX15
  def tails: Stream[Stream[A]] =
    unfold(this) {
      case Empty => None
      case Cons(h,t) => Some((Cons(h,t), t()))
    } append Stream(empty)

  def hasSubsequence[A](s: Stream[A]): Boolean =
    tails exists (_ startsWith s)

  // EX16
  def scanRight[B](z: B)(f: (A,B) => B): Stream[B] = {
    this match {
      case Empty => consz
      case Cons(h,t) => t().scanRight(f(h(),z))(f)
    }
  }

}

println(Stream(1,2,3).tails.map(_.toList).toList)
//println(Stream(1,2,3).zipAll(Stream(1)).toList)
//println(Stream(1,2,4,4).zipWith(Stream(1,2,4,4))(_ + _).toList)
//println(Stream(1,2,3).toList)
//println(Stream(1,2,3).take(2).toList)
//println(Stream(1,2,3).take2(2).toList)
//println(Stream(1,2,3,4).drop(2).toList)
//println(Stream(1,2,3,4).takeWhile(_ < 3).toList)
//println(Stream(1,2,3,4).takeWhile3(_ <= 3).toList)
//println(Stream(1,2,3,4).forAll(_ < 5))
//println(Stream(1,2,3,4).forAll2(_ < 5))
//println(Stream(1,2,3,4).takeWhile2(_ < 3).toList)
//println(Stream(1,2,3,4).headOption2)
//println(Empty.headOption2)
//println(Stream(1,2,3,4).map(_ + 1).toList)
//println(Stream(1,2,3,4).map2(_ + 1).toList)
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

  // EX8
  def constant[A](a: A): Stream[A] = {
    lazy val ct:Stream[A] = Cons(() => a, () => ct)
    ct
  }

  // EX9
  def from(n: Int): Stream[Int] = {
    var _n: Int = n
    lazy val n_strm: Stream[Int] = Cons(
      () => _n, () => {_n +=1; n_strm})
    n_strm
  }

  def from2(n: Int): Stream[Int] =
    cons(n, from(n+1))

  // EX10
  def fibs: Stream[Int] = {
    def go(n: Int, n1: Int): Stream[Int] =
      cons(n, go(n1, n+n1))
    go(0,1)
  }

  // EX11
  def unfold[A, S](z: S)(f: S => Option[(A, S)]): Stream[A] = f(z) match {
    case Some((h, s)) => cons(h, unfold(s)(f))
    case None => empty[A]
  }

  // EX12
  def fibs2: Stream[Int] =
    unfold((0,1)) { case (f0, f1) => Some((f0,(f1,f0+f1))) }

  //def from3(n: Int): Stream[Int] =
  //  unfold(cons(n, empty[Int]))((s:Stream[Int]) => Some(n+1, s))

  def from3(n: Int) =
    unfold(n)(n => Some((n, n+1)))

  //def constant2[A](a: A): Stream[A] =
  //  unfold(cons(a, empty[A]))((s:Stream[A]) => Some(a, s))

  def constant2[A](a: A) =
    unfold(a)(_ => Some((a,a)))

  //def ones2: Stream[Int] =
  //  unfold(cons(1, empty[Int]))((s:Stream[Int]) => Some(1, s))

  def ones2: Stream[Int] =
    unfold(1)(_ => Some((1,1)))

}

//println(constant(2).take(5).toList)
//println(from(2).take(5).toList)
//println(fibs.take(7).toList)
//println(fibs2.take(7).toList)
//println(constant2(2).take(5).toList)
//println(from3(2).take(5).toList)
//println(ones2.take(5).toList)
