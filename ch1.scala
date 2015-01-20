def fib(n: Int): Int = {
  @annotation.tailrec
  def go(n: Int, fib_n_1: Int, fib_n_2: Int): Int = {
    if(n < 2) fib_n_1
    else go(n-1, fib_n_1+fib_n_2, fib_n_1)
  }
  go(n, 1, 0)
}
println(fib(5), fib(6), fib(7))

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
