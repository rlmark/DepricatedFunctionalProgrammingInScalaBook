import scala.annotation.tailrec

sealed trait List[+A]
case object Nil extends List[Nothing]
case class Cons[+A](head: A, tail: List[A]) extends List[A]

object List {
  def sum(ints: List[Int]): Int = ints match {
    case Nil => 0
    case Cons(x, xs) => x + sum(xs)
  }

  def product(ds: List[Double]): Double = ds match {
    case Nil => 1.0
    case Cons(0.0, _) => 0.0
    case Cons(x, xs) => x * product(xs)
  }

  def foldRight[A, B](list: List[A], seed: B)(f: (A, B) => B): B = {
    list match {
      case (Nil) => seed
      case (Cons(head, tail)) => f(head, foldRight(tail, seed)(f)) /// HOW????
    }
  }

  def sum1(list: List[Int]): Int = {
    foldRight(list, 0)((x, y) => x + y)
  }

  def product1(list: List[Int]): Int = {
    foldRight(list, 1)(_ * _)
  }

  def apply[A](as: A*): List[A] =
    if (as.isEmpty) Nil
    else Cons(as.head, apply(as.tail: _*))

  // Exercise 3.2
  def tail[A](list: List[A]): List[A] = {
    list match {
      case (Cons(head, tail)) => tail
      case Nil => Nil
    }
  }

  // Exercise 3.3
  def setHead[A](newItem: A, list: List[A]): List[A] = list match {
    case (Cons(_, tail)) => Cons(newItem, tail)
    case Nil => Cons(newItem, Nil)
  }

  // Exercise 3.4
  def drop1[A](l: List[A], n: Int): List[A] = {
    def loop(decrement: Int, currentList: List[A]): List[A] = {
      if (decrement <= 0) currentList
      else loop(decrement - 1, tail(currentList))
    }
    loop(n, l)
  }

  def drop[A](l: List[A], n: Int): List[A] = {
    if (n <= 0) l
    else drop(tail(l), n - 1)
  }

  // Exercise 3.5
  def dropWhile[A](l: List[A], f: A => Boolean): List[A] = {
    l match {
      case (Nil) => Nil
      case (Cons(head, tail)) => if (f(head)) dropWhile(tail, f)
      else l
    }
  }

  // Exercise 3.6
  def init[A](l: List[A]): List[A] = {
    l match {
      case (Nil) => Nil
      case (Cons(head, Nil)) => Nil
      case (Cons(head1, (Cons(head2, Nil)))) => Cons(head1, Nil)
      case (Cons(head, tail)) => Cons(head, init(tail))
    }
  }

  // Exercise 3.7
  // My answer is no, you could not optimize to break out of product when multiplying by 0.

  // Exercise 3.9
  def length[A](as: List[A]): Int = {
    foldRight(as, 0)((a: A, b: Int) => b + 1)
  }

  // Exercise 3.10
  // For stack overflow, see...
  // length(List(1 to 10000000)) DON'T RUN THIS (again.)

  @tailrec
  def foldLeft[A,B](as: List[A], seed: B)(f: (B, A) => B): B = {
    as match {
      case (Nil) => seed
      case (Cons(head, tail)) => foldLeft(tail, f(seed, head))(f) // Think about this.
    }
  }

//  def foldRight[A, B](list: List[A], seed: B)(f: (A, B) => B): B = {
//    list match {
//      case (Nil) => seed
//      case (Cons(head, tail)) => f(head, foldRight(tail, seed)(f))
//    }
//  }

  // Exercise 3.11
  def sumLeft(list: List[Int]):Int = {
    foldLeft(list, 0)((x,y) => x + y)
  }

  def productLeft(list: List[Int]):Int = {
    foldLeft(list, 1)(_ * _)
  }

  def lengthLeft(list: List[Int]):Int = {
    foldLeft(list,0)((b: Int, a: Int) => a + 1)
  }

  // Exercise 3.12
  def reverse[A](list: List[A]): List[A] = {
    foldLeft(list, Nil: List[A])((b,a) => {
    println("This is a " + a)
      val result = Cons(a, b)
      println(result)
      result
    })
  }

  // Exercise 3.13
  def foldLeftFromFoldRight[A, B](list: List[A], seed: B)(f: (B,A) => B): B = ???
  def foldRightFromFoldLeft[A, B](list: List[A], seed: B)(f: (A,B) => B): B = ???

  // Exercise 3.14
  // Implement append in terms of foldRight or foldLeft

  def append[A](a1: List[A], a2: List[A]): List[A] = a1 match {
    case Nil => a2
    case Cons(h,t) => Cons(h, append(t, a2))
  }

  def append2[A](list1: List[A], list2: List[A]): List[A] = list1 match {
    case Nil => list2
    case(Cons(head,tail)) => Cons(head, foldRight(list2, Nil: List[A])((a,b) => b))
  }

  //  def foldRight[A, B](list: List[A], seed: B)(f: (A, B) => B): B = {
  //    list match {
  //      case (Nil) => seed
  //      case (Cons(head, tail)) => f(head, foldRight(tail, seed)(f))
  //    }
  //  }

}

