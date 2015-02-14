
import scala.{Option => _, _}

sealed trait Option[+A] {
  def map[B](f: A => B): Option[B] = this match {
    case Some(a) => Some(f(a))
    case None => None
  }

// EX1
  def flatMap[B](f: A => Option[B]): Option[B] = map(f) getOrElse None

  def getOrElse[B >: A](default: => B): B = this match {
    case None => default
    case Some(a) => a
  }

  def orElse[B >: A](ob: => Option[B]): Option[B] =
    map (Some(_)) getOrElse ob

  def filter(f: A => Boolean): Option[A] =
    flatMap(a => if(f(a)) Some(a) else None)
}
case class Some[+A](get: A) extends Option[A]
case object None extends Option[Nothing]


def mean(xs: Seq[Double]): Option[Double] =
  if(xs.isEmpty) None
  else Some(xs.sum / xs.length)
//println(mean(Seq(1,2,3)))


// EX2
def variance(xs: Seq[Double]): Option[Double] =
  mean(xs) flatMap (m => mean(xs.map(x => math.pow(x - m, 2))))
//println(variance(Seq(1,2,3)))


def lift[A,B](f: A => B): Option[A] => Option[B] = _ map f


// EX3
def map2[A,B,C](a: Option[A], b: Option[B])(f: (A,B) => C): Option[C] = (a,b) match {
  case (None, _) => None
  case (_, None) => None
  case (Some(x), Some(y)) => Some(f(x,y))
}


// EX4
def sequence[A](a: List[Option[A]]): Option[List[A]] = {
  def go(_a: List[Option[A]], res: List[A]):Option[List[A]] = _a match {
    case Nil => Some(res.reverse)
    case x::xs => x match {
      case Some(v) => go(xs, v::res)
      case None => None
    }
  }
  go(a, Nil)
}
//println(sequence(List(Some(1), Some(2))))
//println(sequence(List(Some(1), Some(2), None)))


// EX5
def traverse[A,B](a: List[A])(f: A => Option[B]): Option[List[B]] = a match {
  case Nil => Some(Nil)
  case h::t => map2 (f(h), traverse(t)(f)) (_::_)
}


def sequence2[A](a: List[Option[A]]): Option[List[A]] =
  traverse(a)(x => x)
//println(sequence(List(Some(1), Some(2))))
//println(sequence(List(Some(1), Some(2), None)))
