sealed trait Listt[+A]

case object Nil extends Listt[Nothing]
case class Cons[+A](head: A, tail: Listt[A]) extends Listt[A]

object Listt {
  def sum(ints: Listt[Int]): Int = ints match {
    case Nil => 0
    case Cons(x,xs) => x + sum(xs)
  }
  def product(ds: Listt[Double]): Double = ds match {
    case Nil => 1.0
    case Cons(0.0, _) => 0.0
    case Cons(x,xs) => x * product(xs)
  }
  def apply[A](as: A*): Listt[A] =
    if (as.isEmpty) Nil
    else Cons(as.head, apply(as.tail: _*))

  // EX2 ///////////
  def tail[A](l: Listt[A]) : Listt[A] = l match {
    case Nil => Nil
    case Cons(_, xs) => xs
  }
  // Regarding the choices to implement 'case Nil', I guess an exception could be raised or an empty list be returned

  // EX3 ///////////
  def drop[A](n: Int, l: Listt[A]) : Listt[A] = (n, l) match {
    case (_, Nil) => Nil
    case (0, l) => l
    case (_n, Cons(_, xs)) => drop(_n-1, xs)
  }

  // EX4 ///////////
  def dropWhile[A](l: Listt[A]) (pred: A => Boolean): Listt[A] = l match {
    case Nil => Nil
    case Cons(x, xs) if (pred(x)) => dropWhile (xs) (pred)
    case _ => l
  }

  // EX5 //////////
  def setHead[A](l: Listt[A], head: A): Listt[A] = l match {
    case Nil => Nil
    case Cons(x, xs) => Cons(head, xs)
  }

  // EX6 /////////
  def init[A](l: Listt[A]): Listt[A] = l match {
    case Nil => Nil
    case Cons(x, Nil) => Nil
    case Cons(x, xs) => Cons(x, init(xs))
  }

}

// EX1
val x = Listt(1,2,3,4,5) match {
  case Cons(x, Cons(2, Cons(4, _))) => x
  case Nil => 42
  case Cons(x, Cons(y, Cons(3, Cons(4, _)))) => x + y
  case Cons(h, t) => h + Listt.sum(t)
  case _ => 101
}
// x: Int = 3, matches case #3
//println(x)

import Listt._
//println(tail(Listt('a', 'b')))
//println(drop(2, Listt(1,2,3)))
//println(drop(1, Nil))
//println(dropWhile(Listt(1,2,3,4,5)) (x => x < 4))
//println(setHead(Listt(1,2,3), 4))
//println(init(Listt(1,2,3,4)))


// EX7
// No, it can't. foldRight does not expose, by means of a parameter, the functionality to stop the recursion.


// EX8
def foldRight[A,B](l: Listt[A], z: B)(f: (A, B) => B): B =
  l match {
    case Nil => z
    case Cons(x, xs) => f(x, foldRight(xs, z)(f))
  }

val something = foldRight(Listt(1,2,4), Nil:Listt[Int]) (Cons(_,_))
//println(something)
// I really do not know at this time


// EX9
def length[A](l: Listt[A]): Int =
  foldRight(l, 0) ((_:A, b:Int) => 1 + b)
//println(length(Listt(1,2,3)))


// EX10
def foldLeft[A,B](l: Listt[A], z: B)(f: (B, A) => B): B = l match {
  case Nil => z
  case Cons(x, xs) => foldLeft(xs, f(z, x)) (f)
}


// EX11
def sum(l: Listt[Int]): Int =
  foldLeft(l, 0) (_+_)

def product(l: Listt[Int]) : Int =
  foldLeft(l, 1) (_*_)

def lengthFL[A](l: Listt[A]) : Int =
  foldLeft(l, 0) ((b:Int, _:A) => b + 1)

//println(sum(Listt(1,2,3)))
//println(product(Listt(1,2,3)))
//println(lengthFL(Listt(1,2,3,4,5)))


// EX12
def reverse[A](l: Listt[A]) : Listt[A] =
  foldLeft(l, Nil:Listt[A]) ((b, a) => Cons(a,b))
//println(reverse(Listt(1,2,3)))


// EX13
//def foldLeftFR[A,B](l: Listt[A], z:B)(f: (B, A) => B): B =
  //foldRight(l, Nil:Listt[A])()


// EX14
def append[A](l: Listt[A], el: A) : Listt[A] =
  foldRight(l, Cons(el, Nil)) ((x: A, xs: Listt[A]) => Cons(x, xs))
//println(append(Listt(1,2,4), 5))
