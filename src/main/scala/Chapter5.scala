sealed trait MyStream[+A]

case class MyCons[+A](h: () => A, t: () => MyStream[A]) extends MyStream[A]

object Chapter5 extends App {
  if2(true, "OK", "NO")
  val x = maybeTwice(true, {
    println(" hi");
    1111
  })
  val x1 = maybeTwice2(true, {
    println("lazy-hi");
    1111
  })


  def square(x: Double): Double = x * x

  def if2[A](cond: Boolean, onTrue: => A, onFalse: => A): A = {
    if (cond) onTrue else onFalse
  }

  def maybeTwice(b: Boolean, i: => Int): Int = {
    if (b) i + i else 0
  }

  def maybeTwice2(b: Boolean, i: => Int): Int = {
    lazy val j = i
    if (b) j + j else 0
  }
}

case object MyEmpty extends MyStream[Nothing]

object MyStream {
  def apply[A](as: A*): MyStream[A] = {
    if (as.)
  }
}