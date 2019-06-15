sealed trait MyEither[+L, +R] {
  final def map[B](f: R => B): MyEither[L, B] = {
    this match {
      case MyLeft(value) => MyLeft(value)
      case MyRight(value) => MyRight(f(value))
    }
  }

  final def map2[LL >: L, B, C](value: MyEither[LL, B])(f: (R, B) => C): MyEither[LL, C] = {
    for {
      e1 <- this
      e2 <- value
    } yield f(e1, e2)
    //    this.flatMap{e1 =>
    //      value.map{e2 =>
    //        f(e1,e2)
    //      }
    //    }
  }

  final def flatMap[LL >: L, B](f: R => MyEither[LL, B]): MyEither[LL, B] = {
    this match {
      case MyLeft(value) => MyLeft(value)
      case MyRight(value) => f(value)
    }
  }

  final def orElse[LL >: L, B >: R](f: => MyEither[LL, B]): MyEither[LL, B] = {
    this match {
      case MyLeft(value) => f
      case MyRight(value) => MyRight(value)
    }
  }
}

// Exe4.1
sealed trait MyOption[+A] {
  def get(): A

  def isEmpty(): Boolean

  def isDefined(): Boolean = !isEmpty()


  // Exe4.2
  final def map2[B, C](myOption: MyOption[B])(f: (A, B) => C): MyOption[C] = {
    this.flatMap { oo1 =>
      myOption.map { oo2 =>
        f(oo1, oo2)
      }
    }
  }

  final def map[B](f: A => B): MyOption[B] = {
    this match {
      case MyNone => MyNone
      case MySome(value) => MySome(f(value))
    }
  }

  final def flatMap[B](f: A => MyOption[B]): MyOption[B] = {
    this match {
      case MyNone => MyNone
      case MySome(value) => f(value)
    }
  }

  final def foreach(f: A => Unit): Unit = {
    this match {
      case MyNone => Unit
      case MySome(value) => f(value)
    }
  }

  final def getOrElse[B >: A](default: => B): B = {
    this match {
      case MyNone => default
      case MySome(value) => value
    }
  }

  final def orElse[B >: A](alternative: => MyOption[B]): MyOption[B] =
    this match {
      case MyNone => alternative
      case _ => this
    }

  final def filter(f: A => Boolean): MyOption[A] = {
    this match {
      case MyNone => MyNone
      case MySome(value) if f(value) => this
    }
  }
}

case class MyLeft[+L](value: L) extends MyEither[L, Nothing]

case class MyRight[+R](value: R) extends MyEither[Nothing, R]

final case class MySome[+A](value: A) extends MyOption[A] {
  override def get(): A = value

  override def isEmpty(): Boolean = false
}

object Chapter4 extends App {
  val lo = MyOption(1) :: MyOption(2) :: MyOption(3) :: Nil
  println(lo)
  val ol = MyOption.sequence(lo)
  println(ol)
  val tra = MyOption.traverse(lo) {
    MyOption(_)
  }
  println(tra)

  val me = IndexedSeq(1.0, 2.0, 3.0)
  val emp = IndexedSeq()
  println(MyEither.mean(emp))

  val left = MyLeft("OK")
  val right = MyRight(0)
  println(right.map(_ + 1))
  //  println(right.orElse(_))
  val e1 = MyLeft("OK")
  val e2 = MyRight(111)
  println(e1.map2(e2) { (a, b) =>
    a
  })
}

object MyEither {

  def traverse[L, R, B](as: List[R])(f: R => MyEither[L, B]): MyEither[L, List[B]] = {
    as match {
      case Nil => MyRight(Nil)
      case head :: tail => f(head).flatMap { nestHead =>
        traverse(tail)(f).map { nestTail =>
          nestHead :: nestTail
        }
      }
    }
  }

  //  def sequence[L, R](as: List[MyEither[L, R]]): MyEither[L, List[R]] = {
  //    as match {
  //      case Nil => MyEither(Nil)
  //      case head :: tail => head.flatMap { nestHead =>
  //        sequence(tail).map { nestTail =>
  //          nestHead :: nestTail
  //        }
  //      }
  //    }
  //  }

  def safeDiv(x: Int, y: Int): MyEither[Exception, Int] = {
    try MyRight(x / y)
    catch {
      case e: Exception => MyLeft(e)
    }
  }

  def mean(xs: IndexedSeq[Double]): MyEither[String, Double] = {
    if (xs.isEmpty)
      MyLeft("OK")
    else
      MyRight(xs.sum / xs.length)
  }
}

object MyOption {
  def apply[A](x: A): MyOption[A] = {
    if (x == null) MyNone else MySome(x)
  }

  //  Exe4.4
  def sequence[A](list: List[MyOption[A]]): MyOption[List[A]] = {
    list match {
      case Nil => MySome(Nil)
      case head :: tail => head.flatMap { nestHead =>
        sequence(tail).map { nestTail =>
          nestHead :: nestTail
        }
      }
    }
  }

  def map2[A, B, C](a: MyOption[A], b: MyOption[B])(f: (A, B) => C): MyOption[C] =
    a flatMap (aa => b map (bb => f(aa, bb)))

  // Exe4.5
  def traverse[A, B](list: List[A])(f: A => MyOption[B]): MyOption[List[B]] =
    list match {
      case Nil => MySome(Nil)
      //      case h::t => map2(f(h), traverse(t)(f))(_ :: _)
      case head :: tail => f(head).flatMap { nestHead =>
        traverse(tail)(f).map { nestTail =>
          nestHead :: nestTail
        }
      }
    }

  def empty[A]: Option[A] = None

  def variance(xs: Seq[Double]): MyOption[Double] = {
    mean(xs).flatMap { m =>
      mean(xs.map { x =>
        math.pow(x - m, 2)
      })
    }
  }

  def mean(xs: Seq[Double]): MyOption[Double] =
    if (xs.isEmpty) MyNone
    else MySome(xs.sum / xs.length)
}

final case object MyNone extends MyOption[Nothing] {
  override def get(): Nothing = throw new NoSuchElementException("存在しません")

  override def isEmpty(): Boolean = true
}

