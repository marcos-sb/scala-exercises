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
def foldLeftFR[A,B](l: Listt[A], z:B)(f: (B, A) => B): B =
  foldRight(reverse(l), z)((a,b) => f(b,a))

def foldRightFL[A,B](l: Listt[A], z:B)(f: (A, B) => B) : B =
  foldLeft(reverse(l), z)((a, b) => f(b, a))

def sum2(l: Listt[Int]) : Int = {
  foldLeftFR(l, 0)(_ + _)
}
def sum3(l: Listt[Int]) : Int = {
  foldRightFL(l, 0)(_ + _)
}
//println(sum2(Listt(1,2,3)))
//println(sum3(Listt(1,2,3)))


// EX14
def append[A](l: Listt[A], el: A) : Listt[A] =
  foldRight(l, Cons(el, Nil)) ((x: A, xs: Listt[A]) => Cons(x, xs))
//println(append(Listt(1,2,4), 5))

def appendL[A](l1: Listt[A], l2: Listt[A]) : Listt[A] =
  foldRight(l1, l2)(Cons(_,_))
//println(appendL(Listt(1,2,4), Listt(5)))


// EX15
def concat[A](l: Listt[Listt[A]]) : Listt[A] = {
  def go(_l: Listt[Listt[A]], out: Listt[A]) : Listt[A] = {
    def aux_go(__l: Listt[A], _out: Listt[A]) : Listt[A] =
      __l match {
        case Nil => _out
        case Cons(x, xs) => aux_go(xs, Cons(x, _out))
      }

    _l match {
      case Nil => out
      case Cons(x, xs) => go(xs, aux_go(x, out))
    }
  }
  reverse(go(l, Nil))
}
//println(concat(Listt(Listt(1,2), Listt(3,4))))

def concat2[A](l: Listt[Listt[A]]) : Listt[A] = {
  foldRight(l, Nil:Listt[A])(appendL)
}
//println(concat2(Listt(Listt(1,2), Listt(3,4))))


// EX16
def add1(l: Listt[Int]) : Listt[Int] =
  foldRight(l, Nil:Listt[Int])((a,b) => Cons(1+a,b))

//val l = Listt(1,2,3)
//println(l, add1(l), l)


// EX17
def str(l: Listt[Double]) : Listt[String] =
  foldRight(l, Nil:Listt[String])((a,b) => Cons(a.toString,b))
//println(str(Listt(1,2,3)))


// EX18
def map[A,B](as: Listt[A])(f:A => B) : Listt[B] =
  foldRight(as, Nil:Listt[B])((x,xs) => Cons(f(x), xs))
//println(map(Listt(1,2,3)) ((a:Int) => a+1))


// EX19
def filter[A](as: Listt[A])(f:A => Boolean): Listt[A] = as match {
  case Nil => Nil
  case Cons(x, xs) if f(x) => Cons(x, filter(xs)(f))
  case Cons(_, xs) => filter(xs)(f)
}
def removeOdds(l: Listt[Int]) =
  filter(l)((a:Int) => a % 2 == 0)
//println(removeOdds(Listt(1,2,3,4,5)))


// EX20
def flatMap[A,B](as: Listt[A])(f:A => Listt[B]) : Listt[B] = as match {
  case Nil => Nil
  case Cons(x, xs) => appendL(f(x),flatMap(xs)(f))
}
//println(flatMap(Listt(1,2,3))(i => Listt(i,i)))


// EX21
def filter2[A](as: Listt[A])(f: A => Boolean) : Listt[A] =
  flatMap(as) ((a:A) => if (f(a)) Cons(a, Nil) else Nil)
//println(filter2(Listt(1,2,3,4)) (a => a % 2 == 0))


// EX22
def listAdd(l1: Listt[Int])(l2: Listt[Int]) : Listt[Int] = (l1, l2) match {
  case (Nil, Nil) => Nil
  case (Nil, Cons(x, xs)) => Cons(x, listAdd(Nil) (xs))
  case (l, Nil) => listAdd(Nil) (l)
  case (Cons(x1, xs1), Cons(x2, xs2)) => Cons(x1 + x2, listAdd(xs1) (xs2))
}
//println(listAdd(Listt(1,2,3,4)) (Listt(1,2,3)))


// EX23
def zipWith[A,B,C](l1: Listt[A])(l2: Listt[B])(f: (A,B) => C) : Listt[C] =
  (l1, l2) match {
    case (Nil, _) => Nil
    case (_, Nil) => Nil
    case (Cons(x1, xs1), Cons(x2,xs2)) => Cons(f(x1, x2), zipWith(xs1)(xs2)(f))
  }
//println(zipWith(Listt(1,2,3)) (Listt(1,2,3)) (_+_))*/


// EX24
def hasSubsequence[A](sup: List[A], sub:List[A]) : Boolean = {
  def go(_sup: List[A], _sub:List[A]) : Boolean = (_sup, _sub) match {
    case (_, Nil) => true
    case (Nil, _) => false
    case (h1::t1, h2::t2) => if(h1==h2) go(t1,t2) else go(t1, sub)
  }
  go(sup, sub)
}
//println(hasSubsequence(List(1,2,3,5,6), List(5,6)))*/
//println(hasSubsequence(List(1,2,3,5,6), List(5,6,7)))*/
