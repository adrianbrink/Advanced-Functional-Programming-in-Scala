// Advanced Programming 2015
// Andrzej Wasowski, IT University of Copenhagen
//
// Scalacheck's user guide:
// https://github.com/rickynils/scalacheck/wiki/User-Guide

// Places to complete are marked ...

package adpro

import org.scalatest.FlatSpec
import org.scalatest.prop.Checkers
import org.scalacheck._
import org.scalacheck.Prop._
import Arbitrary.arbitrary

import language.implicitConversions


class FingerTreeSpecWasowski extends FlatSpec with Checkers {

  import adpro.data._
  import adpro.data.FingerTree._

  // Generator of arbitrary trees of given size for scala check (you can use it
  // in your properties)

  // def fingerTreeOfN[A] (n: Int, gen: Gen[A]) :Gen[FingerTree[A]] = ...
  // generate it using Gen.listOfN of Integers between 0 and 1000, and then map
  // it to finger trees using toTree.

  // Arbitrary trees of size between zero and 100
  //
  // Pick up a generated integer n btw 0 and 100 and then use it to generate a
  // tree of this size using fingerTreeOfN (can be done using flatMap or for
  // comprehensions).
  // def fingerTree[A] (gen: Gen[A]) :Gen[FingerTree[A]] = ...

  // The same as above but as an instance of Arbitrary
  // Uncomment to make available once you have the fingerTree function
  //
  // implicit def arbFingerTree[A] (implicit arb: Arbitrary[A]) =
  //   Arbitrary[FingerTree[A]](fingerTree[A] (arbitrary[A]))



  behavior of "basic FingerTree constructors"

  // Why do we distinguish Empty from Digit() without any contents?
  // You can experiment below with writing different terms (or do it in the
  // REPL)

  it should "compile" in {
    Empty ()
    Single[Int] (1)
    Digit('t')
    Digit('t','h')
    Deep (Digit('t','h'), Empty(), Digit ('r','e','e'))
    Deep[Char] (Digit('t','h'), Empty(), Digit ('r','e','e'))
    Deep(Digit(),Empty(),Digit())
    // the following is the tree from page 4, it seems to render well without
    // any type annotations
    Deep (
      Digit('t','h'),
      Deep(
        Digit(Node2 ('i','s'), Node2('i','s')),
        Empty(),
        Digit(Node3 ('n','o','t'), Node2('a','t'))
      ),
      Digit('r','e','e')
    )
  }

  behavior of "addL"

  it should "produce a queue containing the inserted element" in {
    // assert(Empty().addL(42).toList == List(42))
  }

  // it should "produce a queue containing the inserted elements" in check {
    // forAll (Gen.listOfN(100, Gen.choose[Int](0,1000))) {
    //   (l :List[Int]) =>
    //     l.foldRight[FingerTree[Int]] (Empty()) (FingerTree.addL).toList == l
    // }
  // }

  behavior of "addR"

  // ...

  behavior of "toTree"

  // it should "be an identitity on trees" in check {
    // forAll (fingerTreeOfN(100, Gen.choose[Int](0,1000))) {
    //   (t :FingerTree[Int]) => toTree (t) == t
    // }
  // }

  behavior of "left views (extractors)"

  // the tests can be easily rewritten to paper-style views

  it should "be NilTree on Empty" in {
    // Empty() match {
    //   case NilTree () => assert(Empty().empty)
    //   case _ => fail()
    // }
  }

  it should "be ConsL(_,Nil) on Single" in {
    // Single(42) match {
    //   case ConsL(_,NilTree()) => assert(Single(42).nonEmpty)
    //   case _ => fail()
    // }
  }

  // it should "be ConsL(_,Consl(_,_)) on any tree larger than 3" in check {
    // val ft3plus = Gen.choose(3,100) flatMap { fingerTreeOfN(_,arbitrary[Int]) }
    // forAll (ft3plus) { (t: FingerTree[Int]) => t match {
    //   case ConsL (a, ConsL(b,_)) => true
    //   case _ => false
    //   }
    // }
  // }

  // it should "have the right prefix on any tree larger than 3" in check {
    // val list3plus = Gen.choose(3,100) flatMap { Gen.listOfN(_,arbitrary[Int]) }
    // forAll (list3plus) { (l: List[Int]) =>
    //   val t = Digit.toTree (l)
    //   t.headL == l.head && t.tailL.headL == l.tail.head &&
    //   t.tailL.tailL.headL == l.tail.tail.head
    // }
  // }

  behavior of "right views"

  // ...


}
