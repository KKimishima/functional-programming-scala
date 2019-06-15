import Exe3._

object Chapter3 extends App {
  //  Cons(x, Cons(y, Cons(3, Cons(4, _))))パターンにマッチ
  val exe3_1 = MyList(1, 2, 3, 4, 5)
  println(Exe3_1(exe3_1))
  val exe3_2 = MyList(1, 2, 3)
  Exe3_2.tail(exe3_2).foreach(println)
  val exe3_3 = MyList(1, 2, 3)
  Exe3_3.setHead(exe3_2)(12).foreach(println)
  val exe3_4 = MyList(1, 2, 3)
  val exe3_4_fn = Exe3_4.drop(exe3_4)(_: Int)
  println(exe3_4_fn(0))
  println(exe3_4_fn(1))

  val exe3_5 = MyList(1, 2, 3, 4, 5)
  val exe3_5_Result = Exe3_5.dropWhile(exe3_5) { i =>
    i < 3
  }
  println(exe3_5_Result)

  println(Exe3_6.init(MyList(1, 2, 3)))

  val exe3_foldRight = MyList(1, 2, 3)
  val exe3_foldRightResult = MyList.foldRight(exe3_foldRight, 0) { (x, y) =>
    x + y
  }
  println(exe3_foldRightResult)

  println(Exe3_9.length(MyList(1, 2, 3)))

  val exe3_10_foldLeft = MyList(1, 2, 3, 4, 6)
  val exe3_10_foldLeftResult = Exe3_10.foldLeft(exe3_foldRight, 0) { (x, y) =>
    x + y
  }
  println(exe3_10_foldLeftResult)

  println(Exe3_11.sumInt(MyList(1, 2, 3)))
  println(Exe3_11.length(MyList(1, 2, 3)))

  println(Exe3_12.reverse(MyList(1, 2, 3)))
  println(Exe3_12.reverse2(MyList(1, 2, 3)))

  //  println(Exe3_14.append(MyList(1, 2, 3), MyList(1)))
  println(Exe3_14.append2(MyList(1, 2, 3), MyList(4, 5, 6)))

  println(Exe3_16.add1(MyList(1, 2, 3)))

  println(Exe3_17.doubleToString(MyList(1.1, 1.2)))
  println(Exe3_17.doubleToString2(MyList(1.1, 1.2)))

  val exe3_18 = Exe3_18.map2(MyList(1, 2, 3)) { it =>
    it.toString + "_OK"
  }
  println(exe3_18)

  val exe3_19 = Exe3_19.filter2(MyList(1, 2, 3, 4, 5, 6)) { i =>
    i % 2 == 0
  }
  println(exe3_19)

  val exe3_20 = Exe3_20.flatMap2(MyList(1, 2, 3)) { i =>
    MyList(i, i)
  }
  println(exe3_20)
}

object Exe3 {

  sealed trait MyList[+A]

  case class Cons[+A](head: A, tail: MyList[A]) extends MyList[A]

  case object Nil extends MyList[Nothing]

  object MyList {
    def foreach[A](myList: MyList[A], zero: Unit = Unit)(f: A => Unit): Unit = {
      myList match {
        case Nil => Unit
        case Cons(head, tail) => this.foreach(tail, f(head))(f)
      }
    }

    def apply[A](as: A*): MyList[A] = {
      if (as.isEmpty) Nil
      else Cons(as.head, apply(as.tail: _*))
    }

    def sum(int: MyList[Int]): Int = int match {
      case Nil => 0
      case Cons(head, tail) => head + this.sum(tail)
    }

    def product(ds: MyList[Double]): Double = ds match {
      case Nil => 1.0
      case Cons(0.0, _) => 0.0
      case Cons(head, tail) => head * product(tail)
    }

    def foldRight[A, B](myList: MyList[A], zero: B)(f: (A, B) => B): B = {
      myList match {
        case Nil => zero
        case Cons(head, tail) => f(head, this.foldRight(tail, zero)(f))
      }
    }
  }

}

object Exe3_1 {

  import Exe3._

  def apply(myList: MyList[Int]): Int = myList match {
    //      x,2,4 この数列にマッチ
    case Cons(x, Cons(2, Cons(4, _))) => x
    //      数列が存在しない場合にマッチ
    case Nil => 42
    // x,y,3,4の数列にマッチ
    case Cons(x, Cons(y, Cons(3, Cons(4, _)))) => x + y
    //      上記以外の数列パターン
    case Cons(h, t) => h + MyList.sum(t)
    case _ => 101
  }
}

object Exe3_2 {

  import Exe3._

  def tail[A](myList: MyList[A]): Option[MyList[A]] = myList match {
    case Nil => None
    case Cons(_, tail) => Some(tail)
  }
}

object Exe3_3 {

  import Exe3._

  def setHead[A](myList: MyList[A])(value: A): Option[MyList[A]] = myList match {
    case Nil => None
    case Cons(_, tail) => Some(Cons(value, tail))
  }
}

object Exe3_4 {

  import Exe3._

  def drop[A](myList: MyList[A])(dropNum: Int): MyList[A] = {
    if (dropNum == 0) myList
    else myList match {
      case Nil => Nil
      case Cons(_, tail) => this.drop(tail)(dropNum - 1)
    }

  }
}

object Exe3_5 {

  import Exe3._

  def dropWhile[A](myList: MyList[A])(f: A => Boolean): MyList[A] = {
    myList match {
      case Cons(head, tail) if f(head) => this.dropWhile(tail)(f)
      case _ => myList
    }
  }
}

object Exe3_6 {

  import Exe3._

  def init[A](myList: MyList[A]): MyList[A] = {
    myList match {
      case Nil => sys.error("init of empty list")
      case Cons(_, Nil) => Nil
      case Cons(h, t) => Cons(h, init(t))
    }
  }

  def append[A](myList: MyList[A])(appendMyList: MyList[A]): MyList[A] = {
    myList match {
      case Nil => appendMyList
      case Cons(head, tail) => Cons(head, this.append(tail)(appendMyList))
    }
  }
}

object Exe3_9 {

  import Exe3.MyList._

  def length[A](myList: MyList[A]): Int = {
    foldRight(myList, 0) { (_, acc) =>
      acc + 1
    }
  }
}

object Exe3_10 {

  def foldLeft[A, B](myList: MyList[A], zero: B)(f: (B, A) => B): B = {
    myList match {
      case Nil => zero
      case Cons(head, tail) => this.foldLeft(tail, f(zero, head))(f)
    }
  }
}

object Exe3_11 {

  import Exe3_10._

  def sumInt(myList: MyList[Int], zero: Int = 0): Int = {
    foldLeft(myList, zero) { (x, y) =>
      x + y
    }
  }

  def length[A](myList: MyList[A], zero: Int = 0): Int = {
    foldLeft(myList, zero) { (x, _) =>
      x + 1
    }
  }
}

object Exe3_12 {

  import Exe3._
  import Exe3_10._

  def reverse[A](myList: MyList[A]): MyList[A] = {
    foldLeft(myList, MyList[A]()) { (acc, head) =>
      Cons(head, acc)
    }
  }

  def reverse2[A](myList: MyList[A], zero: MyList[A] = MyList()): MyList[A] = {
    myList match {
      case Nil => zero
      case Cons(head, tail) => this.reverse2(tail, Cons(head, zero))
    }
  }
}

object Exe3_14 {

  import Exe3_10._

  def append[A](myList: MyList[A], appendMyList: MyList[A]): MyList[A] = {
    foldLeft(myList, appendMyList) { (acc, element) =>
      Cons(element, acc)
    }
  }


  def append2[A](myList: MyList[A], appendList: MyList[A]): MyList[A] = {
    myList match {
      case Nil => appendList
      case Cons(head, tail) => Cons(head, this.append2(tail, appendList))
    }
  }

}

object Exe3_16 {

  def add1(myList: MyList[Int], zero: MyList[Int] = MyList()): MyList[Int] = {
    myList match {
      case Nil => zero
      case Cons(head, tail) => Cons(head + 1, this.add1(tail, zero))
    }
  }
}

object Exe3_17 {

  import Exe3.MyList._
  import Exe3.Nil

  def doubleToString(myList: MyList[Double], zero: MyList[String] = MyList()): MyList[String] = {
    myList match {
      case Nil => zero
      case Cons(head, tail) => Cons(head.toString(), this.doubleToString(tail, zero))
    }
  }

  def doubleToString2(myList: MyList[Double]): MyList[String] = {
    foldRight(myList, Nil: MyList[String])((h, t) => Cons(h.toString, t))
  }
}

object Exe3_18 {

  import Exe3.MyList.foldRight

  def map2[A, B](myList: MyList[A])(f: A => B): MyList[B] = {
    myList match {
      case Nil => MyList[B]()
      case Cons(head, tail) => Cons(f(head), map2(tail)(f))
    }
  }

  def map[A, B](myList: MyList[A])(f: A => B): MyList[B] = {
    foldRight(myList, Nil: MyList[B]) { (head, tail) =>
      Cons(f(head), tail)
    }
  }
}

object Exe3_19 {

  import Exe3.MyList.foldRight

  def filter2[A](myList: MyList[A])(f: A => Boolean): MyList[A] = {
    myList match {
      case Nil => Nil
      case Cons(head, tail) if f(head) => this.filter(Cons(head, tail))(f)
      case Cons(_, tail) => this.filter(tail)(f)
    }
  }

  def filter[A](myList: MyList[A])(f: A => Boolean): MyList[A] = {
    foldRight(myList, Nil: MyList[A]) { (head, tail) =>
      if (f(head)) Cons(head, tail)
      else tail
    }
  }
}

object Exe3_20 {

  import Exe3.MyList.foldRight
  import Exe3_14.append

  def flatMap[A, B](myList: MyList[A])(f: A => MyList[B]): MyList[B] = {
    foldRight(myList, Nil: MyList[B]) { (head, tail) =>
      append(f(head), tail)
    }
  }

  def flatMap2[A, B](myList: MyList[A], zero: MyList[B] = Nil)(f: A => MyList[B]): MyList[B] = {
    myList match {
      case Nil => zero
      case Cons(head, tail) => this.flatMap2(tail, f(head))(f)
    }
  }
}


