// Advanced Programming, Exercises by A. Wasowski, IT University of Copenhagen
//
// AUTHOR1:
// AUTHOR2:
// Group number:
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
// To run the compiled file do "scala Tests"
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

// Exercise  1

/* I created OrderedPoint as a trait instead of a class, so I can mix it into
 * Points (this allows me to use java.awt.Point constructors without
 * reimplementing them). As constructors are not inherited, I would have to
 * reimplement them in my subclass.  This is not a problem if I mix in a trait
 * construction time. */
import java.awt.Point

trait Ordered extends Point with scala.math.Ordered[Point]{
  override def compare(that: Point): Int = {
    if(x == that.x) y - that.y
    else x - that.x
  }
}

// Chapter 3

sealed trait Tree[+A]
case class Leaf[A] (value: A) extends Tree[A]
case class Branch[A] (left: Tree[A], right: Tree[A]) extends Tree[A]

object Tree {

  // Exercise 2 (3.25)

  // def size[A] (t :Tree[A]) :Int = {
    def size[A](as: Tree[A]) :Int = {
    as match {
      case Branch(l, r) => 1 + size(l) + size(r)
      case Leaf(x) => 1
    }
  }

  // Exercise 3 (3.26)

    def maximum(as: Tree[Int]) : Int = {
    def max1(bs : Tree[Int], maxInt: Int): Int = bs match {
      case Branch(l,r) => max1(l,maxInt).max(max1(r,maxInt))
      case Leaf(value) => value
    }
    max1(as,Integer.MIN_VALUE)
  }

  // Exercise 4 (3.27)

  def depth[A](as: Tree[A]) : Int ={
    def dep[A](bs: Tree[A], lvl: Int): Int = bs match {
      case Branch(l,r) => dep(l,lvl+1).max(dep(r,lvl+1))
      case Leaf(_) => lvl
    }
    dep(as,0)
  }

  // Exercise 5 (3.28)

  def map[A,B] (as: Tree[A])(f: A => B): Tree[B] = as match {
    case Branch(l,r) => Branch(map(l)(f),map(r)(f))
    case Leaf(value) => Leaf(f(value))
   }


  // Exercise 6 (3.29)

 def fold[A,B] (t: Tree[A])(g: A => B) (f: (B,B) => B)  :B =  {
    t match {
      case Leaf(value) => g(value)
      case Branch(lt, rt) => f(fold(lt)(g)(f), fold(rt)(g)(f))
    }
  }

  def size1[A] (as: Tree[A]):Int ={
    fold(as)(_ => 1)((x, y) => 1 + x + y)
  }
  def maximum1(as: Tree[Int]): Int ={
    fold(as)(p => p)((x, y) => x.max(y))
  }
  def depth1[A](tr: Tree[A]): Int ={
    fold(tr)(a => 1)( (x, y) => y + 1)
  }

  def map1[A,B] (as: Tree[A])(f: A => B): Tree[B] ={
    fold(as)( a => Leaf(f(a)) : Tree[B])((l, r)=> Branch(l, r))
  }
}



case class some[+A] (get: A) extends Option[A]
case object None extends Option[Nothing]
sealed trait Option[+A] {

  // Exercise 7 (4.1)

   def map[B] (f: A => B): Option[B]={

    this match {
      case None => None
      case some(get) => some(f(get))
    }
  }

  // Ignore the arrow in default's type this week
// (it should work (almost) as if it was not there)

def flatMap[B] (f:A => Option[B]) : Option[B]={
    this match {
      case some(x) => f(x)
      case None => None
    }
  }

  def getOrElse[B >: A] (ob: => B): B={
    this match {
      case None => ob
      case some(x) => x
    }
  }
  def orElse[B >: A](ob: => Option[B]) : Option[B] ={
    map(x=> some(x)).getOrElse(ob)
  }
  def filter(f: A => Boolean) : Option[A]={

    flatMap(x => if(f(x))some(x) else None)
  }
}





object ExercisesOption {

  // Remember that mean is implemented in Chapter 4 of the text book

  def mean(xs: Seq[Double]): Option[Double] =
    if (xs.isEmpty) None
    else some(xs.sum / xs.length)

  // Exercise 8 (4.2)

    def variance(xs: Seq[Double]): Option[Double] = {
    mean(xs) flatMap (m => mean(xs map (x => math.pow(x - m, 2))))
}
  // Exercise 9 (4.3)
    def map2[A, B, C](a: Option[A], b: Option[B])(f: (A, B) => C): Option[C] = {
    // a flatMap(aa => b flatMap(bb => some(f(aa,bb))))
    for (aa <- a;
         bb <- b)
      yield (f(aa, bb))
  }

  // Exercise 10 (4.4)

  // def sequence[A] (aos: List[Option[A]]) : Option[List[A]] = ...

  // Exercise 11 (4.5)

  // def traverse[A,B] (as: List[A]) (f :A => Option[B]) :Option[List[B]] =

}


// Test cases for running in the compiled vesion (uncomment as you go, or paste
// them into REPL in the interactive version)

object Tests  {
 def main() {
   // Exercise 1
   // val p = new java.awt.Point(0,1) with OrderedPoint
   // val q = new java.awt.Point(0,2) with OrderedPoint
   // assert(p < q)

   // Notice how we are using nice infix comparison on java.awt
   // objects that were implemented way before Scala existed :) (And without the
   // library implementing a suitable comparator). We did not have to recompile
   // java.awt.Point


   // Exercise 2
   assert(Tree.size(Branch(Leaf(1), Leaf(2))) == 3)
   // Exercise 3
   assert(Tree.maximum(Branch(Leaf(1), Leaf(2))) == 2)
   //Exercise 4
   val t4 = Branch(Leaf(1), Branch(Branch(Leaf(2), Leaf(3)), Leaf(4)))
   assert(Tree.depth(t4) == 3)
   //Exercise 5
   val t5 = Branch(Leaf("1"), Branch(Branch(Leaf("2"), Leaf("3")), Leaf("4")))
   assert(Tree.map(t4)(_.toString) == t5)

   // Exercise 6
    assert (Tree.size1 (Branch(Leaf(1), Leaf(2))) == 3)
    assert (Tree.maximum1 (Branch(Leaf(1), Leaf(2))) == 2)
    assert (Tree.depth1 (t4) == 3)
    assert (Tree.map1 (t4) (_.toString) == t5)

   // Exercise 7
      assert (some(1).map (x => x +1) == some(2))
      assert (some(41).getOrElse(42) == 41)
      assert (None.getOrElse(42) == 42)
      assert (some(1).flatMap (x => some(x+1)) == some(2))
      assert ((None: Option[Int]).flatMap[Int] (x => some(x+1)) == None)
      assert (some(41).orElse (some(42)) == some(41))
      assert (None.orElse (some(42)) == some(42))
      assert (some(42).filter(_ == 42) == some(42))
      assert (some(41).filter(_ == 42) == None)
      assert ((None: Option[Int]).filter(_ == 42) == None)

   // Exercise 8
      assert (ExercisesOption.variance (List(42,42,42)) == some(0.0))
      assert (ExercisesOption.variance (List()) == None)


   // Exercise 9
      assert (ExercisesOption.map2 (some(42),some(7)) (_ + _) == some(49))
      assert (ExercisesOption.map2 (some(42),None) (_ + _) == None)
      assert (ExercisesOption.map2 (None: Option[Int],some(7)) (_ + _) == None)
      assert (ExercisesOption.map2 (None: Option[Int],None) (_ + _) == None)

   // Exercise 10
   // assert (ExercisesOption.sequence (List(some(1), some(2), some(42))) == some(List(1,2,42)))
   // assert (ExercisesOption.sequence (List(None,    Some(2), Some(42))) == None)
   // assert (ExercisesOption.sequence (List(some(1), None,    some(42))) == None)
   // assert (ExercisesOption.sequence (List(some(1), some(2), None    )) == None)

   // Exercise 11
   // def f (n: Int) :Option[Int] = if (n%2 == 0) Some(n) else None
   // assert (ExercisesOption.traverse (List(1,2,42)) (some(_)) == some(List(1,2,42)))
   // assert (ExercisesOption.traverse (List(1,2,42)) (f) == None)

 }

}
