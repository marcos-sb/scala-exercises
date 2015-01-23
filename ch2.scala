// EX1
def fib(n: Int): Int = {
  @annotation.tailrec
  def go(n: Int, fib_n_1: Int, fib_n_2: Int): Int = {
    if(n < 2) fib_n_1
    else go(n-1, fib_n_1+fib_n_2, fib_n_1)
  }
  go(n, 1, 0)
}
println(fib(5), fib(6), fib(7))


// EX2
def isSorted[A](as: Array[A], cmpfun: (A,A) => Boolean): Boolean = {
  @annotation.tailrec
  def go(i: Int, len: Int): Boolean = {
    if(i < len)
      if(cmpfun(as(i-1), as(i))) go(i+1, len) else false
    else true
  }
  go(1, as.length)
}
println(isSorted(Array(1,2,3), (a:Int,b:Int) => a < b))
println(isSorted(Array(1,1,3), (a:Int,b:Int) => a < b))


// EX3
def partial1[A,B,C](a: A, f: (A,B) => C): B => C = {
  (b:B) => f(a,b)
}
def intadd(a:Int, b:Int): Int = {
  a + b
}
println(partial1(1, intadd) (3))


// EX4
def curry[A,B,C](f: (A,B) => C): A => (B => C) = {
  (a:A) => (b:B) => f(a,b)
}
println(curry(intadd) (1) (3))


// EX5
def uncurry[A,B,C](f: A => B => C): (A, B) => C = {
  (a:A, b:B) => f(a) (b)
}
def curry_intadd(a:Int) : Int => Int = {
  (b:Int) => intadd(a,b)
}
println(curry_intadd (3) (6))
println(uncurry(curry_intadd) (3,6))


// EX6
def compose[A,B,C](f: B => C, g: A => B) : A => C = {
  (a:A) => f (g (a))
}
def curry_intmult(a:Int) : Int => Int = {
  (b:Int) => a * b
}
println(compose(curry_intadd(2), curry_intmult(3)) (5))
