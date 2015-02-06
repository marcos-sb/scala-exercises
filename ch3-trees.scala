sealed trait Tree[+A]
case class Leaf[A](value: A) extends Tree[A]
case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]


// EX25
def size[A](t: Tree[A]) : Int = t match {
  case Leaf(_) => 1
  case Branch(l, r) => size(l) + size(r) + 1
}
//println(size(Leaf(2)))
//println(size(Branch(Leaf(2), Branch(Leaf(3), Leaf(4)))))

def size2[A](t:Tree[A]) : Int = {
  def go(_t: Tree[A], sum: Int) : Int= _t match {
    case Leaf(_) => sum + 1
    case Branch(l, r) => go(l, go(r, sum + 1))
  }
  go(t, 0)
}
//println(size2(Leaf(2)))
//println(size2(Branch(Leaf(2), Branch(Leaf(3), Leaf(4)))))


// EX26
def maximum(t: Tree[Int]) : Int = t match {
  case Leaf(v) => v
  case Branch(l, r) => maximum(l) max maximum(r)
}
//println(maximum(Branch(Leaf(2), Branch(Leaf(3), Leaf(4)))))
//println(maximum(Branch(Leaf(21), Branch(Leaf(3), Leaf(4)))))

def maximum2(t: Tree[Int]) : Int = {
  def go(_t: Tree[Int], maxi: Int) : Int = _t match {
    case Leaf(v) => v max maxi
    case Branch(l, r) => go(l, go(r, maxi))
  }
  go(t, 0)
}
//println(maximum(Branch(Leaf(2), Branch(Leaf(3), Leaf(4)))))
//println(maximum(Branch(Leaf(21), Branch(Leaf(3), Leaf(4)))))


// EX27
def depth[A](t: Tree[A]) : Int = t match {
  case Leaf(_) => 0
  case Branch(l, r) => 1 + (depth(l) max depth(r))
}
//println(depth(Branch(Leaf(21), Branch(Leaf(3), Leaf(4)))))


// EX28
def map[A,B](t: Tree[A])(f: A => B) : Tree[B] = t match {
  case Leaf(v) => Leaf(f(v))
  case Branch(l,r) => Branch(map(l)(f), map(r)(f))
}
//println(map(Branch(Leaf(21), Branch(Leaf(3), Leaf(4))))(1+_))


// EX29 (cpy-pasted, let's be fair ;)
def fold[A,B](t: Tree[A])(f: A => B)(g: (B,B) => B): B = t match {
  case Leaf(v) => f(v)
  case Branch(l,r) => g(fold(l)(f)(g), fold(r)(f)(g))
}

def size3[A](t: Tree[A]) : Int =
  fold(t)(v => 1)((l,r) => 1 + l + r)
//println(size3(Leaf(2)))
//println(size3(Branch(Leaf(2), Branch(Leaf(3), Leaf(4)))))

def maximum3[A](t: Tree[Int]) : Int =
  fold(t)(v => v)(_ max _)
//println(maximum3(Branch(Leaf(2), Branch(Leaf(3), Leaf(4)))))
//println(maximum3(Branch(Leaf(21), Branch(Leaf(3), Leaf(4)))))

def depth2[A](t: Tree[A]) : Int =
  fold(t)(v => 0)((l, r) => 1 + (l max r))
//println(depth2(Branch(Leaf(21), Branch(Leaf(3), Leaf(4)))))

def map2[A,B](t: Tree[A])(f: A => B) : Tree[B] =
  fold(t)(v => Leaf(f(v)): Tree[B])(Branch(_,_))
//println(map(Branch(Leaf(21), Branch(Leaf(3), Leaf(4))))(1+_))
