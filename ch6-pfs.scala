def rollDice: Int = {
  val rng = new scala.util.Random
  rng.nextInt(6)
}

def rollDice2(rng: scala.util.Random): Int = {
  rng.nextInt()
}

// ^- internal state -> not purely functional

trait RNG {
  def nextInt: (Int, RNG)
}

case class SimpleRNG(seed: Long) extends RNG {
  def nextInt:(Int, RNG) = {
    val newSeed = (seed * 0x5DEECE66DL + 0xBL) & 0xFFFFFFFFFFFFL
    val nextRNG = SimpleRNG(newSeed)
    val n = (newSeed >>> 16).toInt
    (n, nextRNG)
  }

  def randomPair(rng: RNG): ((Int, Int), RNG) = {
    val (i1,rng2) = rng.nextInt
    val (i2,rng3) = rng2.nextInt
    ((i1,i2), rng3)
  }


}

object SimpleRNG {

  //EX1
  def nonNegativeInt(rng: RNG): (Int, RNG) = {
    val rndInt = rng.nextInt
    if(rndInt._1 == Int.MinValue)
      nonNegativeInt(rng)
    else {
      val mask = (1 << 31) - 1
      (rndInt._1 & mask, rndInt._2)
    }
  }

  //EX2
  def double(rng: RNG): (Double, RNG) = {
    val rndInt = nonNegativeInt(rng)
    if(rndInt._1 == 0)
      double(rndInt._2)
    else
      (rndInt._1 / Int.MaxValue.toDouble, rndInt._2)
  }

  //EX3
  def intDouble(rng: RNG): ((Int, Double), RNG) = {
    val rndInt = rng.nextInt
    val rndDouble = double(rndInt._2)
    ((rndInt._1, rndDouble._1), rndDouble._2)
  }

  def doubleInt(rng: RNG): ((Double, Int), RNG) = {
    val intDoub = intDouble(rng)
    ((intDoub._1._2, intDoub._1._1), intDoub._2)
  }

  def double2(rng: RNG): ((Double, Double, Double), RNG) = {
    val d1 = double(rng)
    val d2 = double(d1._2)
    val d3 = double(d2._2)
    ((d1._1, d2._1, d3._1), d3._2)
  }

  //EX4
  def ints(count: Int)(rng: RNG): (List[Int], RNG) = count match {
    case n if n >= 1 => {
      val rnd = rng.nextInt
      (rnd._1 :: (ints(n-1)(rnd._2)._1), rnd._2)
    }
    case _ => (Nil, rng)
  }

  def ints2(count: Int)(rng: RNG): (List[Int], RNG) = {
    def go(_n: Int, rndList: List[Int], rng: RNG): (List[Int], RNG) = _n match {
      case n if n >= 1 => {
        val rnd = rng.nextInt
        go(n-1, rnd._1 :: rndList, rnd._2)
      }
      case _ => (rndList, rng)
    }
    go(count, Nil, rng)
  }
}

//println(SimpleRNG.ints(3)(SimpleRNG(28))._1)
//println(SimpleRNG.ints2(3)(SimpleRNG(28))._1)
