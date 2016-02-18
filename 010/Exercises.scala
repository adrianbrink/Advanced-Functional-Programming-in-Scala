// FINAL
// 
// Advanced Programming, Exercises by A. Wąsowski, IT University of Copenhagen
//
// AUTHOR1: Andrea Bitzili - abit@itu.dk
// AUTHOR2: Adrian Brink - adbr@itu.dk
//
// Write names and ITU email addresses of both group members that contributed to
// the solution of the exercise (in alphabetical order by family name).
//
// You should work with the file by following the associated exercise sheet
// (available in PDF from the course website).
//
// The file is meant to be compiled as follows:
//
// scalac Exercises.scala
//
// or
//
// fsc Exercises.scala
//
// To run the compiled file do "scala Exercises"
//
// To load the file int the REPL start the 'scala' interpreter and issue the
// command ':load Exercises.scala'. Now you can interactively experiment with
// your code.
//
// Continue solving exercises in the order presented in the PDF file. Large
// parts of the file are commented in order to make sure that the exercise
// compiles.  Uncomment and complete fragments as you proceed.  The file shall
// always compile and run after you are done with each exercise (if you do them
// in order).  Please compile and test frequently.

// The extension of App allows writing statements at class top level (the so
// called default constructor). For App objects they will be executed as if they
// were placed in the main method in Java.

object Exercises extends App {

  // Exercise 3

  def power (x: Double, n: Int) :Double = {
    if (n == 0) 1.0
    else if (n < 0) 1.0 / power (x, n)
    else if (n > 0 && n % 2 != 0) x * power (x, n-1)
    else power (x, n/2) * power (x, n/2)
  }

  // A few tests, uncomment when your implementation is ready.

  assert (power (2.0, 2) == 4.0)
  assert (power (1.0, 42) == 1.0)
  //
  // The above assertions should pass when you call "scala Exercises".
  //
  // The following one should fail. Uncomment to check that assert works as
  // expected:
  //
  // assert (power (1.0) (42) == 2.0)

  // add 2-3 more tests:
  //
  // ...

  // Exercise 4

  def fib (n: Int) :Int = {
    @annotation.tailrec
    def helper(n: Int, next: Int, accumulated: Int) :Int = {
      if (n == 0) accumulated
      else helper (n-1, accumulated + next, next)
    }
    helper (n, 1, 0)
  }

  // some tests (uncomment, add more):

  // assert (fib (1) == 0)
  // ...

  // Exercise 5

  // A simple object describing a cost line; implemented imperatively, Java
  // style (this way until we learn more Scala)
  class Expense {

    // A constructor definition
    def this (tag :String, price :Int) = {
      this()
      this.tag = tag
      this.price = price
    }

    var tag   :String = "" // a tag line in the accounting system
    var price :Int    = 0 // the price is in cents
  }

  // computes the total of expenses in cents

  def total (expenses: Array[Expense]) :Int = {
    @annotation.tailrec
    def helper(exp: Int, sum: Int) :Int = {
      if (exp < 0) sum
      else helper(exp - 1, expenses(exp).price + sum)
    }
    helper(expenses.length - 1, 0)
  }

  val testcase1 = Array[Expense](
    new Expense("Coffee", 450),
    new Expense("Cake", 350) )

  assert (total (testcase1) == 800) // uncomment

  // Add one or two more tests
  // ...


  // Exercise 6

  def isSorted (as: Array[Int], ordered: (Int,Int) =>  Boolean) :Boolean = {
    def helper(x: Int) :Boolean = {
      if (x == as.length - 1) true
      else if (ordered(as(x), as(x+1))) helper(x+1)
      else false
    }
    helper(0)
  }

  def ordered (x :Int, y :Int) :Boolean = {
    if (x <= y) true
    else false
  }



  // some tests (uncomment)

  // assert ( isSorted (Array(1,2,3,4,5,6), (a: Int, b: Int)=> a <= b))
  // assert (!isSorted (Array(6,2,3,4,5,6), (a: Int, b: Int)=> a <= b))
  // assert (!isSorted (Array(1,2,3,4,5,1), (a: Int, b: Int)=> a <= b))

  // add two tests with another type, for example an Array[String]

  // Exercise 7: a curried version of solution to exercise 3

  // def power1(x: Double) (n: Int) :Double = ...
  def power1 (x: Double) (n: Int) :Double = {
    if (n == 0) 1.0
    else if (n < 0) 1.0 / power1 (x) (n)
    else if (n > 0 && n % 2 != 0) x * power1 (x) (n-1)
    else power1 (x) (n/2) * power1 (x) (n/2)
  }

  // Exercise 8

  def curry[A,B,C] (f: (A,B) => C) : A => (B => C) = {
    (a :A) => (b :B) => f(a, b) :C
  }
  //
  // test if it type checks by currying power automatically:

  val power_curried: Double => Int => Double = curry(power)

  assert (power_curried (1.0) (42) == 1.0)

  // Exercise 9

  def uncurry[A,B,C] (f: A => B => C) : (A,B) => C = {
    (a :A, b :B) => f(a)(b) :C
  }

  val power_uncurried: (Double,Int) => Double = uncurry(power1)

  assert (power_uncurried (1.0, 42) == 1.0)

  // Exercise 10

  def compose[A,B,C] (f: B => C, g: A => B) : A => C = {
    (a :A) => f(g(a :A)) :C
  }

}
