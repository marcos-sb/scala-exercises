
import scala.{Option => _, Either => _, _}

sealed trait Option[+A] {
  def map[B](f: A => B): Option[B] = this match {
    case Some(a) => Some(f(a))
    case None => None
  }

  def flatMap[B](f: A => Option[B]): Option[B] = map(f) getOrElse None

  def getOrElse[B >: A](default: => B): B = this match {
    case None => default
    case Some(a) => a
  }

  def orElse[B >: A](ob: => Option[B]): Option[B] =
    map (Some(_)) getOrElse ob

  def filter(f: A => Boolean): Option[A] =
    flatMap(a => if(f(a)) Some(a) else None)

  def lift[A,B](f: A => B): Option[A] => Option[B] = _ map f
}
case class Some[+A](get: A) extends Option[A]
case object None extends Option[Nothing]

def mean(xs: Seq[Double]): Option[Double] =
  if(xs.isEmpty) None
  else Some(xs.sum / xs.length)
//println(mean(Seq(1,2,3)))

def variance(xs: Seq[Double]): Option[Double] =
  mean(xs) flatMap (m => mean(xs.map(x => math.pow(x - m, 2))))
//println(variance(Seq(1,2,3)))
