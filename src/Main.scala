import scala.annotation.tailrec
import List._

object Main {
  def main(args: Array[String]): Unit = {

    //Exercise 2.1
    def fibonacciNonTailRec(n: Int): Int= {
      if (n == 1) 0
      else if  (n == 2) 1
      else fibonacciNonTailRec(n - 1) + fibonacciNonTailRec(n - 2)
    }
    println(fibonacciNonTailRec(12))

    def fibonacciTailRec(n: Int): Int = {
      @tailrec
      def loop(currentN: Int, a: Int, b: Int): Int = {
        if (currentN == n) b
        else loop(currentN + 1, b, a + b)
      }
      loop(1, 1, 0)
    }
    println(fibonacciTailRec(12))

    // Exercise 2.2
    def isSorted[A](as: Array[A], ordered: (A,A) => Boolean): Boolean = {
      def loop(head: A, tail: Array[A]): Boolean = {
        if (head == Nil) true
        else if (!ordered(head, tail.head)) false
        else loop(tail.head, tail.tail)
      }
      loop(as.head, as.tail)
    }

    // Exercise 2.3
    def curry[A,B,C](f: (A, B) => C): A => (B => C) = {
      (a: A) => (b: B) => f(a,b)
    }

    // Exercise 2.4
    def uncurry[A,B,C](f: A => B => C): (A, B) => C = {
      (a: A, b: B) => f(a)(b)
    }

    // Exercise 2.5
    def compose[A,B,C](f: B => C, g: A => B): A => C = {
      (a: A) => f(g(a))
    }

    // Exercise 2.6

    val x = List(1,2,3,4,5) match {
      case Cons(x, Cons(2, Cons(4, _))) => x
      case Nil => 42
      case Cons(x, Cons(y, Cons(3, Cons(4, _)))) => x + y
      case Cons(h, t) => h + sum(t)
      case _ => 101
    }
    // answer is 3.

    // Exercise 3.2 see List object.
    println(tail(List(1,2,3,4)))

    // Exercise 3.3
    println(setHead(41,List(2,3,4,5,6)))

    // Exercise 3.4
    println(drop1(List(1,2,3,4,5,6,7),5))
    println(drop(List(1,2,3,4,5,6,7),5))
    println(drop(List(1,2),4))

    // Exercise 3.5
    def f(i: Int): Boolean = {
      i == 5
    }
    def y(i: Int): Boolean = {
      i < 5
    }
    println(dropWhile(List(2,3,4,5,6), f))
    println(dropWhile(List(2,3,4,5,6), y))

    // Exercise 3.6
    println(init(List(1,2,3,4,5)))
    println(init(List(1)))
    println(init(List()))

    // Exercise 3.8
    println(foldRight(List(1,2,3,4,5), Nil:List[Int])(Cons(_,_)))
    // What do you think this says about the relationship between foldRight and the data constructors of List?
    // FoldRight can be used to construct lists?

    // Exercise 3.9
    println(length(List(1,2,3,4,5)))
    println(length(List()))

    // Exercise 3.11
    println(sumLeft(List(1,2,3)))

    // Exercise 3.12
    println(reverse(List(1,2,3)))

    // Exercise 3.14
    // Implement append using foldLeft or foldRight
    println("HI THIS IS WHERE YOU ARE")
    println(append(List(1,2,3), List(10,11,12)))
    println(append2(List(1,2,3), List(10,11,12)))
    println(append3(List(1,2,3), List(10,11,12)))
    println(append4(List(1,2,3), List(10,11,12)))




  }

}
