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
      case (Cons(head, tail)) =>
        val foldRightOnTail: B = foldRight(tail, seed)(f)
        f(head, foldRightOnTail) /// HOW????
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
  // length(List(1 to 10000000)) DON'T RUN THIS

  @tailrec
  def foldLeft[A,B](as: List[A], seed: B)(f: (B, A) => B): B = {
    as match {
      case (Nil) => seed
      case (Cons(head, tail)) => foldLeft(tail, f(seed, head))(f) // Think about this.
    }
  }

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
    case Cons(head, tail) => Cons(head, tail)
    case(Cons(head,Nil)) => Cons(head, foldRight(list2, Nil: List[A])((a,b) => Cons(a,b)))
  }

  // Close but no cigar
  def append3[A](list1: List[A], list2: List[A]): List[A] = list1 match {
    case Nil => list2
    case(Cons(head,tail)) => Cons(head, foldRight(list2, Nil: List[A])((a,b) => Cons(a,b)))
  }

  def append4[A](list1: List[A], list2: List[A]): List[A] = {
    // maybe make the seed of list 1 the result of a foldRight on list 2...???
    val seedIsL2 = foldRight(list2, Nil: List[A])((x,y)=> Cons(x,y))
    foldRight(list1, seedIsL2)((a,b)=> Cons(a,b))
    // YASSSS!!!
  }

  def append5[A](list1: List[A], list2: List[A]): List[A] = {
    foldRight(list1, list2)((a,b) => Cons(a,b))
  }

  // Exercise 3.15
  // Concatenate a list of lists into a single list
  def concatenateLists[A](listOfLists: List[List[A]]):List[A] = listOfLists match {
    case Nil => Nil
    case (Cons(miniList, tail)) => append(foldRight(miniList, Nil: List[A])((a,b)=>Cons(a,b)), concatenateLists(tail))
  }

  // Exercise 3.16
  // Write a function that transforms a list of integers by adding 1 to each element.
  def add1(list: List[Int]): List[Int] = list match {
    case Nil => Nil
    case Cons(head: Int, tail: List[Int]) => Cons(head + 1, add1(tail))
  }

  def add1Fold(list: List[Int]): List[Int] = {
    foldRight(list, Nil: List[Int]){
      case ( i: Int, outputList) =>
        val v = i + 1
        Cons(v, outputList)
    }
  }

  // Exercise 3.17
  // Write a function that turns each value in a List[Double] into a String.

  def doubleToString(list: List[Double]): List[String] = list match {
    case Nil => Nil
    case Cons(head, tail) => Cons(head.toString, doubleToString(tail))
  }

  def doubleToStringFold(list: List[Double]): List[String] = {
    foldRight(list, Nil: List[String]){
      case(d: Double, outputList) =>
        val string: String = d.toString
        Cons(string, outputList)
    }
  }

  // Exercise 3.18
  // Write a function that generalizes modifying each element in a list while maintaining the structure of the list.
  def map[A, B](list : List[A])(f: A => B): List[B] = list match {
    case Nil => Nil
    case Cons(head, tail) => Cons(f(head), map(tail)(f))
  }
  // Note, this implementation is not tail recursive

  def mapWithFold[A,B](list: List[A])(f: A => B): List[B] = {
    foldRight(list, Nil: List[B]){
      case(item: A, outputList: List[B]) =>
        val mutated: B = f(item)
        Cons(mutated, outputList)
    }
  }

  // Exercise 3.19
  def filter[A](as: List[A])(f: A => Boolean): List[A] = as match {
    case Nil => Nil
    case Cons(head, tail) =>
      if (f(head) == true) Cons(head,filter(tail)(f))
      else foldRight(tail, Nil: List[A])((a, list) => filter(tail)(f))
  }




}

