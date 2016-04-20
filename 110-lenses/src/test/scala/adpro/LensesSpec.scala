// Advanced Programming
// Andrzej Wasowski, IT University of Copenhagen

// NOTE: It is exected that two tests fail (one for l1 and one for l2) If you
// uncommend the second test for l3, it will fail, too (by design). The other
// tests should pass.

package adpro

import org.scalatest.{FunSuite,FlatSpec}
import org.scalatest.prop.Checkers
import org.scalacheck._
import org.scalacheck.Prop._
import org.scalacheck.Arbitrary
import org.scalacheck.Arbitrary._

import monocle._
import monocle.{Lens,Optional} // we prefer these from Monocle than Scalaz
import scalaz._
import Scalaz._
import scalaz.scalacheck.ScalazArbitrary._

import adpro.Lenses._




class LensesSpec extends FlatSpec with Checkers {

  // Exercise 1.
  // Write the laws polymorphically for *any* total lens and instantiate it for
  // concrete lenses.

  // def PutGet[C,A] (l: Lens[C,A])
  //   (implicit aA: Arbitrary[A], aC: Arbitrary[C]) :Prop = TODO

  // def GetPut[C,A] ... TODO

  // def PutPut[C,A] ... TODO

  // specification of a total lense laws (refers to the laws above)
  // def wellBehavedTotalLense[A,C] (l: Lens[C,A])
  //   (implicit ac: Arbitrary[C], aa: Arbitrary[A]) = {
  //   it should "obey the PutGet law" in check { PutGet (l) }
  //   it should "obey the GetPut law" in check { GetPut (l) }
  // }

  // def veryWellBehavedTotalLense[A,C] (l: Lens[C,A])
  //   (implicit aC: Arbitrary[C], aA: Arbitrary[A]) = {
  //     it should behave like wellBehavedTotalLense (l)
  //     it should "obey the PutPut law" in check { PutPut (l) }
  // }
  //
  // Calling above tests for l1 l2 and l3 (just uncomment and run)

  // "l1" should behave like wellBehavedTotalLense (l1) // will fail GetPut see p. 6
  // "l2" should behave like wellBehavedTotalLense (l2) // will fail PutGet see p. 6
  // "l3" should behave like wellBehavedTotalLense (l3)
  // // it should behave like veryWellBehavedTotalLense (l3)

  // "codiag[Int]" should behave like veryWellBehavedTotalLense (codiag[Int])
  // "codiag[String]" should behave like veryWellBehavedTotalLense (codiag[String])
  // "codiag1[Int]" should behave like veryWellBehavedTotalLense (codiag1[Int])
  // "codiag1[String]" should behave like veryWellBehavedTotalLense (codiag1[String])



  // Tests for Exercise 4 (uncomment)

  // "itu" should "show Alex at zipcode 2800" in
  // { assertResult(itu.students("Alex").zipcode) ("2800") }

  // "itu1" should "show Alex at zipcode 9100" in
  // { assertResult(itu1.students("Alex").zipcode) ("9100") }



  // Test for Exercise 5 (uncomment as needed)

  // "itu2" should "show Alex at zipcode 9100" in
  // { assertResult(itu2.students("Alex").zipcode) ("9100") }



  // Test for Exercise 6 (uncomment as needed)

  // "itu3" should "have all the countries in upper case" in
  // { assert (itu3.students.values.map(_.country).forall (s => s.toUpperCase == s)) }



  // Tests for Exercise 8 (to be completed as Exercise 9)

  // write the laws polymorphically for *any* partial lens and instantiate it for
  // concrete lenses.  The partial comaprison is defined as \sqsubseteq by
  // Foster et al. on page 5 (bottom).
  //
  // Interestingly, Optionals are only partial in the Get function, not in the
  // Put function (have no idea why...). This simplifies laws a bit, but
  // complicates implementations of Optionals. Consequently, the members of
  // Optionals l are l.getOption and l.set (unlike for lenses, where these where
  // l.get and l.set).

  // def PartialPutGet[C,A] (l: Optional[C,A]) = ...

  // def PartialGetPut[C,A] = ...

  // def PartialPutPut[C,A] = ...

  // specification of Optional laws (uncomment)
  //
  // def wellBehavedPartialLense[A,C] (l: Optional[C,A])
  //   (implicit ac: Arbitrary[C], aa: Arbitrary[A]) = {
  //   it should "obey the PartialPutGet law" in check { PartialPutGet (l) }
  //   it should "obey the PartialGetPut law" in check { PartialGetPut (l) }
  // }

  // def veryWellBehavedPartialLense[A,C] (l: Optional[C,A])
  //   (implicit aC: Arbitrary[C], aA: Arbitrary[A]) = {
  //     it should behave like wellBehavedPartialLense (l)
  //     it should "obey the PartialPutPut law" in check { PartialPutPut (l) }
  // }

  //  "setIth" should behave like veryWellBehavedPartialLense (setIth[Int](5))
  //  "setIth1" should behave like veryWellBehavedTotalLense (setIth1[Int](5,-1)) // fails GetPut as expected
}


// One good thing about using Monocle (or any other ready made lens framework
// that we can quickly test wheter our implemented lenses fullfil the laws using
// preimplemented tests.
//
// They do not implement PutPut though.
// Find the laws for Lenses here:
// https://github.com/julien-truffaut/Monocle/blob/master/core/src/main/scala/monocle/law/LensLaws.scala
//
// Find the laws for Optionals here:
// https://github.com/julien-truffaut/Monocle/blob/master/core/src/main/scala/monocle/law/OptionalLaws.scala

// import org.typelevel.discipline.scalatest.Discipline
//
// class LensesSuite extends FunSuite with Discipline {
//
//   import scalaz._
//   import Scalaz._
//
//   checkAll("codiag[Int]", monocle.law.discipline.LensTests(codiag[Int]))
//   checkAll("l1", monocle.law.discipline.LensTests(l1))
//   checkAll("l2", monocle.law.discipline.LensTests(l2))
//   checkAll("l3", monocle.law.discipline.LensTests(l3))
//
//   checkAll("setIth", monocle.law.discipline.OptionalTests(setIth[Int] (5)))
//   checkAll("setIth1", monocle.law.discipline.LensTests(setIth1[Int] (13,-1)))
//
// }
