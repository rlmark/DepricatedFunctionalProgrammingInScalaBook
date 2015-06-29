object MyModule {
  // Exercise 2.2
  def absoluteVal(n: Int): Int = {
    if (n < 0) -n
    else n
  }

  def factorial(n: Int): Int = {
    def decrement(count: Int, acc: Int):Int = {
      if (count <= 0) acc
      else decrement(count-1, acc * count)
    }
    decrement(n, 1)
  }

  // refactor these two format functions into one using a higher order function
  private def formatAbs(x: Int) = {
    // note, the %d and &n are not replaceable with other letters.
    val message = "The absolute value of %d is %d"
    message.format(x, absoluteVal(x))
  }

  private def formatFactorial(x: Int) = {
    val message = "The factorial of %d is %d"
    message.format(x, factorial(x))
  }

  private def formatFunction(x: Int, name: String, f: Int => Int) = {
    val message = "the %s of %d is %d"
    message.format(name, x, f(x))
  }

  def main(args: Array[String]): Unit = {
    println(formatAbs(-42))
    println(formatFactorial(7))
    println(formatFunction(7, "factorial", factorial))
  }


}
