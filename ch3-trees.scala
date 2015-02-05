sealed trait Tree[+A]
case class Leaf[A](value: A) extends Tree[A]
case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]


// EX26
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


// EX27
def maximum(t: Tree[Int]) : Int = {
  def go(_t: Tree[Int], maxi: Int) : Int = _t match {
    case Leaf(v) => v max maxi
    case Branch(l, r) => go(l, go(r, maxi))
  }
  go(t, 0)
}
//println(maximum(Branch(Leaf(2), Branch(Leaf(3), Leaf(4)))))
//println(maximum(Branch(Leaf(21), Branch(Leaf(3), Leaf(4)))))


// EX28
def depth[A](t: Tree[A]) : Int = t match {
  case Leaf(_v) => 0
  case Branch(l, r) => 1 + (depth(l) max depth(r))
}
//println(depth(Branch(Leaf(21), Branch(Leaf(3), Leaf(4)))))


// EX29
def map[A,B](t: Tree[A])(f: A => B) : Tree[B] = t match {
  case Leaf(v) => Leaf(f(v))
  case Branch(l,r) => Branch(map(l)(f), map(r)(f))
}
println(map(Branch(Leaf(21), Branch(Leaf(3), Leaf(4))))(1+_))
