// Advanced Programming
// Andrzej Wasowski, IT University of Copenhagen

package fpinscala.laziness
import scala.language.higherKinds

import org.scalatest.FlatSpec
import org.scalatest.prop.Checkers
import org.scalacheck._
import org.scalacheck.Prop._
import Arbitrary.arbitrary

// If you comment out all the import lines below, then you test the Scala
// Standard Library implementation of Streams. Interestingly, the standard
// library streams are stricter than those from the book, so some laziness tests
// fail on them :)

import stream00._    // uncomment to test the book solution
//import stream01._ // uncomment to test the broken headOption implementation
//import stream02._ // uncomment to test another version that breaks headOption

class StreamSpecAbitAdbr extends FlatSpec with Checkers {

  import Stream._
  // An example generator of random finite non-empty streams
  def list2stream[A] (la :List[A]): Stream[A] = la.foldRight (empty[A]) (cons[A](_,_))

 def cons[A](hd: => A, tl: => Stream[A]): Stream[A] = {
    lazy val head = hd
    lazy val tail = tl
    Cons(() => head, () => tail)
  } 
  //create a Map method since the  properties of the Map in Stream00, Stream01, Stream02 are under test
  def mapStream[A, B] (s: Stream[A])(f: A => B) : Stream[B] = {
    s match {
      case Cons(h, t) => Stream.cons(f(h()), mapStream(t())(f)) 
      case Empty => Stream.empty
    }
  }
  //create an Append method since the append method defined at Stream00, Stream01, Stream 02 are under test
  def append[A](sa: Stream[A], sb: Stream[A]): Stream[A] = sa match {
    case Cons(h, t) => cons[A](h(), append(t(), sb))
    case Empty => sb 
  }
  
  // In ScalaTest we use the check method to switch to ScalaCheck's internal DSL
  
  
  def genNonEmptyStream[A] (implicit arbA :Arbitrary[A]) :Gen[Stream[A]] =
    for { la <- arbitrary[List[A]] suchThat (_.size > 20)}
  yield list2stream (la)
  
    behavior of "headOption"
  
    // a property test:
    it should "return None on an empty Stream (01)" in {
      assert(empty.headOption  == None)
    }
    it should "return the head of the stream packaged in Some (02) " in check {
    implicit def arbIntStream = Arbitrary[Stream[Int]] (genNonEmptyStream[Int])
      // the implict makes the generator available in the context
       ("singleton" |:
         Prop.forAll { (n :Int) => cons (n,empty).headOption == Some (n) } ) &&
       ("random" |:
         Prop.forAll { (s :Stream[Int]) => s.headOption != None } )
    }
    it should "not force the tail" in check {
    implicit def arbIntStream = Arbitrary[Stream[Int]] (genNonEmptyStream[Int])
      ("random" |:
        Prop.forAll { (s :Stream[Int], n :Int) => Stream.cons(n, mapStream[Int, Int](s)(_ / 0)).headOption; true } )
    }

    behavior of "take" 
  
    it should "not force any heads nor tails of the stream it manipulates (03)" in check {
    implicit def arbIntStream = Arbitrary[Stream[Int]] (genNonEmptyStream[Int])
     ("Stream with arithmetic exceptions" |:
       Prop.forAll{ (s :Stream[Int], n :Int) => (n > 0) ==> ((mapStream[Int, Int](s)(_ / 0)).take(n)).isInstanceOf[Stream[Int]]  } )
   }
   it should "not force the n + 1 element (even if we are force all elements of take(n) (04))" in check { 
    implicit def arbIntStream = Arbitrary[Stream[Int]] (genNonEmptyStream[Int])
     Prop.forAll{ (s :Stream[Int], n: Int, n1: Int, n2: Int) => (((cons(n1, cons(n2, cons(n, mapStream[Int, Int](s)( _ / 0 ))))).take(3)).toList).isInstanceOf[List[Int]] }
   }

   it should "Stream.take(n).take(n) == Stream.take(n) for s :Stream and n :Int (05)" in check { 
    implicit def arbIntStream = Arbitrary[Stream[Int]] (genNonEmptyStream[Int])
     Prop.forAll{ (s :Stream[Int], n: Int) => (n > 0) ==> ((s.take(n)).take(n).toList equals (s.take(n)).toList) } 
   }

   behavior of "drop"

   it should "s.drop(n).dop(m) == s.drop(n + m) (06)" in check{
     implicit def arbPositiveInt = Arbitrary[Int] (Gen.choose(1, 20000))
    implicit def arbIntStream = Arbitrary[Stream[Int]] (genNonEmptyStream[Int])
     Prop.forAll{ (s: Stream[Int], n :Int, m :Int) =>  ((s.drop(n).drop(m)).toList equals (s.drop(m + n)).toList) }
   }
   it should "not force any dropped elements (07)" in check {
    implicit def arbSmallInt = Arbitrary[Int] (Gen.choose(1,25)) 
    implicit def arbIntStream = Arbitrary[Stream[Int]] (genNonEmptyStream[Int])
     Prop.forAll { (s: Stream[Int], n: Int) =>  ((mapStream[Int, Int](s)( _ / 0).drop(n)).isInstanceOf[Stream[Int]]) }
   }
    it should "not force any dropped elements even if we force the elements in the tail (08)" in check {
    def genStreamSize[A](size: Int)(genA: Gen[A]): Gen[Stream[A]] =  
      for {
      la <- Gen.listOfN[A](size, genA) 
      }yield(list2stream(la))
    
    // The funciton 'f' maps only the first 'size' elements of the stream
    def genStreamAppend[A](size :Int, f: A => A)(genA: Gen[A])(implicit arbA: Arbitrary[A]) :Gen[Stream[A]] = 
      for {
        sz <- genStreamSize[A](size)(genA)
        s  <- genNonEmptyStream[A]
      }yield append[A](mapStream(sz)(f), s)
    
    implicit def abrStreamAppend = Arbitrary[Stream[Int]] (genStreamAppend[Int](20, (e: Int) => e /0)(Gen.choose(-1000,10000))) 
     Prop.forAll { (s: Stream[Int]) =>  ((s.drop(20)).toList).isInstanceOf[List[Int]] }
   }

   behavior of "map"

   it should " Stream.map(x => x) == Stream (10)" in check {
    implicit def arbIntStream = Arbitrary[Stream[Int]] (genNonEmptyStream[Int])
     Prop.forAll{ (s: Stream[Int]) => (s.map(x => x)).toList equals  s.toList}
   }
    // Can we check if a stream is infinite without forcing the evaluation of the entire stream?
   it should " terminate on infinite streams (11)" in check{
     def infiniteStream :Stream[Int] = cons( 1, infiniteStream)
      Prop.forAll {(n: Int) => infiniteStream.map(x => x) ; true }  
   }

   behavior of "append" 
   
   it should "Strea1.append(Stream2).toList should be equal to (Stream1.toList).append((Stream2.toList))" in check {
     //we assume that the append in the List works as expected
    implicit def arbIntStream = Arbitrary[Stream[Int]] (genNonEmptyStream[Int])
    Prop.forAll{(s1 :Stream[Int], s2 :Stream[Int]) => ((s1.toList).++(s2.toList)) == (s1.append(s2)).toList }
   } 
   it should "not force either of the streams it concatenates" in check {
    implicit def arbIntStream = Arbitrary[Stream[Int]] (genNonEmptyStream[Int])
    Prop.forAll{ (s1 :Stream[Int], s2 :Stream[Int]) => {
      ((mapStream[Int, Int](s1)(e => e/0)).append(mapStream[Int, Int](s2)(e => e/0))).isInstanceOf[Stream[Int]]//To return a Boolean
    }
    }
    
}
