// FINAL
//
// Advanced Programming, Exercises by A. Wąsowski, IT University of Copenhagen
//
// AUTHOR1: Andrea Bitzili - abit@itu.dk
// AUTHOR2: Adrian Brink - adbr@itu.dk
// Group number: 43
//
// meant to be compiled, for example: fsc Stream.scala

package fpinscala.laziness

import Stream._

sealed trait Stream[+A] {

  def headOption () :Option[A] =
    this match {
      case Empty => None
      case Cons(h,t) => Some(h())
    }

  def tail :Stream[A] = this match {
      case Empty => Empty
      case Cons(h,t) => t()
  }

  def foldRight[B] (z : =>B) (f :(A, =>B) => B) :B = this match {
      case Empty => z
      case Cons (h,t) => f (h(), t().foldRight (z) (f))
      // Note 1. f can return without forcing the tail
      // Note 2. this is not tail recursive (stack-safe) It uses a lot of stack
      // if f requires to go deeply into the stream. So folds sometimes may be
      // less useful than in the strict case
    }

  // Note 1. eager; cannot be used to work with infinite streams. So foldRight
  // is more useful with streams (somewhat opposite to strict lists)
  def foldLeft[B] (z : =>B) (f :(A, =>B) =>B) :B = this match {
      case Empty => z
      case Cons (h,t) => t().foldLeft (f (h(),z)) (f)
      // Note 2. even if f does not force z, foldLeft will continue to recurse
    }

  def exists (p : A => Boolean) :Boolean = this match {
      case Empty => false
      case Cons (h,t) => p(h()) || t().exists (p)
      // Note 1. lazy; tail is never forced if satisfying element found this is
      // because || is non-strict
      // Note 2. this is also tail recursive (because of the special semantics
      // of ||)
    }

    def toList(): List[A] = {
      @annotation.tailrec
      def go(s: Stream[A], acc: List[A]): List[A] = s match {
        // Why does Cons(h(), acc) give me a type error but still
        case Cons(h, t) => go(t(), h()::acc)
        case _ => acc
      }
      go(this, List()).reverse
    }

    // What's the difference between Empty and empty?
    def take(n: Int): Stream[A] = this match {
      case Cons(x, xs) if n > 1 => Cons(() => x(), () => xs().take(n-1))
      case Cons(x, _) if n == 1 => cons(x(), Empty)
      case _ => Empty
    }

    @annotation.tailrec
    final def drop(n: Int): Stream[A] = this match {
      case Cons(_, xs) if n > 0 => xs().drop(n-1)
      case _ => this
    }

    def takeWhile(f: A => Boolean): Stream[A] = this match {
      case Cons(x, xs) if f(x()) => cons(x(), xs().takeWhile(f))
      case _ => empty
    }

    def forAll(p: A => Boolean): Boolean = {
      foldRight(true)((a, b) => p(a) && b)
    }

    def takeWhile1(f: A => Boolean): Stream[A] = {
      foldRight(empty[A])((x, xs) =>
        if (f(x)) cons(x, xs)
        else empty)
    }

    def headOption1(): Option[A] = {
      foldRight(None: Option[A])((x, _) => Some(x))
    }

    def map[B](p: A => B): Stream[B] = {
      foldRight(empty[B])((x, xs) => cons(p(x), xs))
    }

    def filter(p: A => Boolean): Stream[B] = {
      foldRight(empty[A])((x, xs) => 
        if (p(x)) cons(x, xs)
        else xs)
    }

    def append[B>:A](s: => Stream[B]): Stream[B] = {
      foldRight(s)((x, xs) => cons(x, xs))
    }

    def flatMap[B](f: A => Stream[B]): Stream[B] = {
      foldRight(empty[B])((x, xs) => f(x) append xs)
    }


  //def find (p :A => Boolean) :Option[A] = this.filter (p).headOption
}

case object Empty extends Stream[Nothing]
case class Cons[+A](h: ()=>A, t: ()=>Stream[A]) extends Stream[A]


object Stream {

  def empty[A]: Stream[A] = Empty

  def cons[A] (hd: => A, tl: => Stream[A]) :Stream[A] = {
    lazy val head = hd
    lazy val tail = tl
    Cons(() => head, () => tail)
  }

  def apply[A] (as: A*) :Stream[A] =
    if (as.isEmpty) empty
    else cons(as.head, apply(as.tail: _*))
    // Note 1: ":_*" tells Scala to treat a list as multiple params
    // Note 2: pattern matching with :: does not seem to work with Seq, so we
    //         use a generic function API of Seq
  
  def from(n: Int): Stream[Int] = {
    cons(n, from(n+1))
  }

  // def to(n: Int): Stream[Int] = {
  //   val l1: Stream[Int] = cons(1, empty)
  //   if n > 0 {

  //   }
  //   cons(n, to(n-1)).reverse
  //   def go(s: Stream[A], n: Int) {
  //     s match {
  //       case Cons()
  //     }
  //   }
  // }
  
  val fibs = {
    def fib(val1: Int, val2: Int): Stream[Int] = {
      cons(val1, fib(val1, val1+val2))
    }
    fib(0, 1)
  }
}

// vim:tw=0:cc=80:nowrap
