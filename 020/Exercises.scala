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

// An ADT of Lists

// Solution 1
// Prints case 3, hence 3

sealed trait List[+A]
case object Nil extends List[Nothing]
case class Cons[+A](head: A, tail: List[A]) extends List[A]


object List {

  // override function application to provide a factory of lists (convenience)

  def apply[A](as: A*): List[A] = // Variadic function
    if (as.isEmpty) Nil
    else Cons(as.head, apply(as.tail: _*))

  // Exercise 2

  def tail[A] (as: List[A]) :List[A] = as match {
    case Nil => Nil
    case Cons(_, xs) => xs
  }

         // dsajdlas  // Exercise 3

  def setHead[A] (as: List[A], newHead: A) : List[A] = as match {
    case Nil => Nil
    case Cons(_, xs) => Cons(newHead, xs)
  }

  // Exercise 4

  def drop[A] (l: List[A], n: Int) : List[A] = {
    if (n <= 0) l
    else l match {
      case Nil => Nil
      case Cons(_, xs) => drop(xs, n-1)
    }
  }

  // Exercise 5

  def dropWhile[A](l: List[A], f: A => Boolean): List[A] = l match {
    case Cons(x, xs) if f(x) => dropWhile(xs, f)
    case _ => l
  }

  // Exercise 6

  def init[A](l: List[A]): List[A] = l match {
    case Nil => Nil
    case Cons(_, Nil) => Nil
    case Cons(x, xs) => Cons(x, init(xs))
  }

  // Exercise 7 is in the bottom of the file

  // Exercise 8

  def foldRight[A,B] (as :List[A], z: B) (f : (A,B)=> B) :B = as match {
    case Nil => z
    case Cons (x,xs) => f (x, foldRight (xs, z) (f))
  }

  def length[A] (as: List[A]): Int = foldRight(as, 0) ((_,acc) => acc + 1)

  // Exercise 9
  @annotation.tailrec
  def foldLeft[A,B] (as: List[A], z: B) (f: (B, A) => B) : B = as match {
    case Nil => z
    case Cons(x, xs) => foldLeft(xs, f(z, x)) (f)
  }

  // Exercise 10

  def sum (as : List[Int]) : Int = foldLeft(as, 0)(_ + _)
  def product (as :List[Int]) : Int = foldLeft(as, 1)(_ * _)
  def length1 (as :List[Int]) : Int = foldLeft(as, 0)((acc, _) => acc + 1)

  // Exercise 11

  def reverse[A] (as :List[A]) :List[A] = foldLeft(as, List[A]()) ((xs, x) => Cons(x, xs))

  // Exercise 12

  def foldRight1[A,B] (as: List[A], z: B) (f: (A, B) => B) : B = {
    foldLeft(reverse(as), z) ((b, a) => f(a, b))
  }

  def foldLeft1[A,B] (as: List[A], z: B) (f: (B,A) => B) : B = {
    foldRight(as, (b: B) => b)((a, g) => b => g(f(b, a)))(z)
  }

  // Exercise 13

  def append[A](a1: List[A], a2: List[A]): List[A] = a1 match {
    case Nil => a2
    case Cons(h,t) => Cons(h, append(t, a2))
  }

  def concat[A] (as: List[List[A]]) :List[A] = {
    foldRight(as, Nil:List[A]) (append)
  }

  // Exercise 14

  def map[A,B] (as :List[A]) (f :A => B) :List[B] = {
    foldRight(as, Nil:List[B]) ((x, xs) => Cons(f(x),xs))
  }

  // Exercise 15 (no coding)

  // Exercise 16

  def filter[A] (as: List[A]) (f: A => Boolean) : List[A] = {
    foldRight(as, Nil:List[A])((x, xs) => if (f(x)) Cons(x, xs) else xs)
  }

  // Exercise 17

  def flatMap[A,B](as: List[A])(f: A => List[B]) : List[B] = {
    concat(map(as)(f))
  }

  // Exercise 18

  def filter1[A] (l: List[A]) (p: A => Boolean) :List[A] = {
    flatMap(l)(a => if (p(a)) List(a) else Nil)
  }

  // Exercise 19

  def add (l: List[Int]) (r: List[Int]): List[Int] = (l, r) match {
    case (Nil, _) => Nil
    case (_, Nil) => Nil
    case (Cons(x1, xs1), Cons(x2, xs2)) => Cons(x1+x2, add(xs1, xs2))
  }

  // Exercise 20

  def zipWith[A,B,C] (f : (A,B)=>C) (l: List[A], r: List[B]) : List[C] = (l, r) match {
    case (Nil, _) => Nil
    case (_, Nil) => Nil
    case (Cons(x1, xs1), Cons(x2, xs2)) => Cons(f(x1, x2), zipWith(f)(xs1, xs2))
  }

  // Exercise 21

  def hasSubsequence[A] (sup: List[A], sub: List[A]) :Boolean = sup match {
    case Nil => sub == Nil
    case _ if startsWith(sup, sub) => true
    case Cons(x, xs) => hasSubsequence(xs, sub)
  }

  def startsWith[A] (a: List[A], b: List[A]) :Boolean = (a, b) match {
    case (_, Nil) => true
    case (Cons(x, xs), Cons(x1, xs1)) if (x == x1) => startsWith(xs, xs1)
  }

  // Exercise 22

  // def pascal (n :Int) : List[Int] = {
  //   def go(l: Int, r: Int) :Int = {
  //   }
  // }
  // a test: pascal (4) = Cons(1,Cons(3,Cons(3,Cons(1,Nil))))
}

// Exercise 7

object Exercise7 extends App {

  case class SalaryLine(name: String, amount: Integer)

  def maximumSalary (b: List[SalaryLine]) :Integer = {
    def go(a: List[SalaryLine]) (max: Integer) :Integer = {
      a match {
        case Nil => max
        case Cons(x, xs) => go(xs) (Math.max(x.amount, max))
      }
    }
    go(b) (-1)
  }

  val test_case = List( SalaryLine("John",41),
    SalaryLine("Alice", 42),
    SalaryLine("Bob",40))

  assert (maximumSalary(test_case) == 41)

}
