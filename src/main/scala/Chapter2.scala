import scala.annotation.tailrec

object Chapter2 extends App {
  println(Exe2_1.fib(10))
  println(Exe2_2.isSorted[Int](Array(1, 2, 3), { (p1, p2) => p1 == p2 }))
  val exe2_3 = Exe2_3.curry[Int, Double, String] { (a, b) => a.toString + b.toString }
  println(exe2_3(1)(1.2))
  val exe2_4 = Exe2_4.uncurry[Int, Double, String](i => b => i.toString + b.toString)
  println(exe2_4(2, 2.2))
  val exe2_5 = Exe2_5.compose[Int, Double, String]({ b => b.toString }, { a => a.toDouble })
  println(exe2_5(2))
}

object Exe2_1 {
  def fib(n: Int): Int = {
    @tailrec
    def loop(n: Int, per: Int, cor: Int): Int = {
      if (n == 0) per
      else loop(n - 1, cor, per + cor)
    }

    loop(n, 0, 1)
  }
}

object Exe2_2 {
  def isSorted[A](as: Array[A], orderd: (A, A) => Boolean): Boolean = {
    @tailrec
    def loop(n: Int): Boolean = {
      if (n >= as.length - 1) true
      else if (orderd(as(n), as(n + 1))) false
      else loop(n + 1)
    }

    loop(0)
  }
}

object Exe2_3 {
  def curry[A, B, C](f: (A, B) => C): A => B => C =
    a => b => f(a, b)
}

object Exe2_4 {
  def uncurry[A, B, C](f: A => B => C): (A, B) => C =
    (a, b) => f(a)(b)
}

object Exe2_5 {
  def compose[A, B, C](f: B => C, g: A => B): A => C =
    a => f(g(a))
}
