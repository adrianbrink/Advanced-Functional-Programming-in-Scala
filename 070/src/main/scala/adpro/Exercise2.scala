// Advanced Programming 2015
// Andrzej Wasowski, IT Univesity of Copenhagen

package adpro

import fpinscala.state._
import fpinscala.state.RNG._
import fpinscala.testing._

import adpro.Exercise1._
import Gen._


object Exercise85_TestCases {

  // The functions requested by the exercise should be written
  // in the object Gen in Gen.scala. Here we have the test cases.
  // Use the in the REPL

  // uncomment once functions are implemented
  //
  // val r1 = Gen.unit(3.14).toStream(42).take(5).toList
  // val r2 = Gen.boolean.toStream(42).take(5).toList
  // val r3 = Gen.boolean.listOfN(3).toStream(42).take(5).toList
}

object Exercise88_TestCases {

  // val unfair_coin =
  //      weighted[Boolean] (Gen.unit (true)->1.0, Gen.unit (false)->2.0)

  // val unfair_coin_stream = unfair_coin.toStream (42)

  // trying checkRatio on large numbers reveals that  this generator is very
  // bad.  The convergense is slow, and the precision does not exceed two digits
  // even for many thousands of tosses. I wonder whether it is our Boolean
  // generator or the bug is somewhere else.  I won't debug this further right
  // now as this is not essential for the class.

  //def checkRatio (n :Int) :Double =
  //  unfair_coin_stream.take(n).partition {x => x} match
  //  { case (x,y) => x.length / (x.length.toDouble + y.length) }

}
