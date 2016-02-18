// Advanced Programming 2015
// Andrzej Wąsowski, IT University of Copenhagen

object Imperative {

  def factorial (n :Int) :Int = {
    var result = 1
    for (i <- 2 to n)
      result *= i
    return result
  }

}

object Functional {

  def factorial (n :Int) :Int =
    if (n<=1) 1
    else n * factorial (n-1)
}

object TailRecursive {

  def factorial (n :Int) = {
    def f (n :Int, r :Int) :Int =
      if (n<=1) r
      else f (n-1, n*r)
    f (n,1)
  }
}


object Main extends App {
  println ("Imperative:" + Imperative.factorial(5))
  println ("Applicative:" + Functional.factorial(5))
  println ("Tail recursive:" + TailRecursive.factorial(5))
}
