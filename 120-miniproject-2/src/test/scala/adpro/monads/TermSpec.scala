// Advanced Programming
// Andrzej Wasowski, IT University of Copenhagen
//
// Simple scenario based tests for term evaluators of Wadler
// (actually there is only two tests, that are taken directly from the paper)
// (more could be taken from later sections)

package adpro.monads

import scala.language.higherKinds

import org.scalatest.FlatSpec
import org.scalatest.Matchers._
import org.scalatest.prop.Checkers
import org.scalacheck._
import org.scalacheck.Prop._
import Arbitrary.arbitrary


class  TermSpec extends FlatSpec with Checkers {

  // test cases, also from Section 2.1 [Wadler]:

  val answer = Div (
                  Div (Cons (1972), Cons (2)),
                  Cons (23)
               )
  val error = Div ( Cons (1), Cons (0) )
  val const = Cons (42)


  // generator of some random divisions
  val genSafeTerm :Gen[Term] = {
    implicit val int100 = Arbitrary[Int](Gen.choose[Int](0,999))
    arbitrary[List[Int]].suchThat(l => l.nonEmpty && l.forall (_!=0)).
      flatMap[List[Term]] ( li =>
        li.sorted.map (Cons compose Math.abs) ).
      flatMap[Term] ( lc =>
        lc.tail.foldLeft[Term] (lc.head) (Div)
  )}

  val genUnsafeTerm :Gen[Term] = {
    implicit val int100 = Arbitrary[Int](Gen.choose[Int](0,999))
    arbitrary[List[Int]].suchThat(l => l.nonEmpty && l.forall (_!=0)).
      flatMap[List[Term]] ( li =>
        li.sorted.map (Cons compose Math.abs) ).
      flatMap[Term] ( lc =>
        lc.foldRight[Term] (Cons(0)) (Div)
  )}

  // Section 2.1

  behavior of "Basic eval"
  it should "answer 42 to a division [Wadler]" in {
    BasicEvaluator.eval (answer) shouldBe 42 }
  it should "return 42 from a constant " in {
    BasicEvaluator.eval (const) shouldBe 42 }
  it should "throw a scala exception on division by 0" in
  { intercept[java.lang.ArithmeticException] {
    BasicEvaluator.eval (error) } }
  it should "crash on unsafe terms" in check {
    forAll (genUnsafeTerm) ( (t: Term) => {
      intercept[java.lang.ArithmeticException] {
      BasicEvaluator.eval (t)}; true }
    )
  }

//  // Section 2.2
//
//  behavior of "Exception eval"
//  it should "answer Return(42) to our division [Wadler]" in
//  { ExceptionEvaluator.eval (answer) shouldBe ExceptionEvaluator.Return(42) }
//  it should "answer Return(42) to a constant" in
//  { ExceptionEvaluator.eval (const) shouldBe ExceptionEvaluator.Return(42) }
//  it should "return an exception value for a division by zero" in
//  { ExceptionEvaluator.eval (error) shouldBe a [ExceptionEvaluator.Raise] }
//
//  // Section 2.3 [Wadler] Variation two: State
//
//  behavior of "State eval"
//  it should "should count two divisions" in check {
//    forAll { (n: Int) =>
//      StateEvaluator.eval (answer).step (n) == (42,n+2) } }
//
//  it should "should count no divisions" in check {
//    forAll { (n: Int) =>
//      StateEvaluator.eval (const).step (n) == (42,n) } }
//
//  it should "throw a scala exception on division by 0" in
//  { intercept[java.lang.ArithmeticException] {
//    StateEvaluator.eval (error).step (0) } }
//
//  // Section 2.4 [Wadler] Variation three: Output
//
//  val result = "eval(Cons(1972)) <= 1972\n" +
//               "eval(Cons(2)) <= 2\n" +
//               "eval(Div(Cons(1972),Cons(2))) <= 986\n" +
//               "eval(Cons(23)) <= 23\n" +
//               "eval(Div(Div(Cons(1972),Cons(2)),Cons(23))) <= 42\n"
//
//  behavior of "Output eval (answer)"
//  it should "give good 'result' and string output" in {
//    val r = OutputEvaluator.eval(answer)
//    r.a shouldBe 42
//    r.o shouldBe result
//  }
//  it should "return simple result for a constant" in {
//    val r = OutputEvaluator.eval(const)
//    r.a shouldBe 42
//    r.o shouldBe "eval(Cons(42)) <= 42\n"
//  }
//  it should "throw a scala exception on division by 0" in
//  { intercept[java.lang.ArithmeticException] {
//    OutputEvaluator.eval (error) } }
//
//
//
//  // Section 2.6 [Wadler] Variation zero, revisited: The basic evaluator
//
//  behavior of "Basic monadic eval"
//
//  it should "be 42 [Wadler]" in
//  { BasicEvaluatorWithMonads.eval (answer).a shouldBe 42 }
//  it should "return 42 for a constant " in {
//    BasicEvaluatorWithMonads.eval (const).a shouldBe 42 }
//
//  it should "throw an exception" in
//  { intercept[java.lang.ArithmeticException]
//  { BasicEvaluatorWithMonads.eval (error) } }
//
//  it should "crash on unsafe terms" in check {
//    forAll (genUnsafeTerm) ( (t: Term) => {
//      intercept[java.lang.ArithmeticException] {
//      BasicEvaluatorWithMonads.eval (t).a}; true }
//    )
//  }
//
//
//
//  behavior of "Basic evalutors"
//
//  it should "behave identically" in check {
//    forAll (genSafeTerm) ( (t: Term) =>
//      BasicEvaluatorWithMonads.eval (t).a == BasicEvaluator.eval(t))
//  }
//
//
//  // Section 2.7 [Wadler] Variation one, revisited: Exceptions
//
//  behavior of "Monadic exception eval"
//
//  it should "be Return(42) [Wadler]" in {
//    ExceptionEvaluatorWithMonads.eval (answer) shouldBe
//      ExceptionEvaluatorWithMonads.Return(42)
//  }
//
//  it should "return an exception on division by zero" in {
//    (ExceptionEvaluatorWithMonads.eval (error)) shouldBe
//      a [ExceptionEvaluatorWithMonads.Raise]
//  }
//
//  def toBasic (m: ExceptionEvaluatorWithMonads.M[Int])
//    : ExceptionEvaluator.M[Int] =  m match {
//      case ExceptionEvaluatorWithMonads.Raise (s) => ExceptionEvaluator.Raise(s)
//      case ExceptionEvaluatorWithMonads.Return (a) => ExceptionEvaluator.Return(a)
//  }
//
//
//
//  behavior of "Exception evalutors"
//
//  it should "behave identically (safe)" in check {
//    forAll (genSafeTerm) ( (t: Term) =>
//      toBasic(ExceptionEvaluatorWithMonads.eval (t)) ==
//        ExceptionEvaluator.eval(t))
//  }
//
//  it should "behave identically (unsafe)" in check {
//    forAll (genUnsafeTerm) ( (t: Term) =>
//      toBasic(ExceptionEvaluatorWithMonads.eval (t)) ==
//        ExceptionEvaluator.eval(t))
//  }
//
//  // Section 2.8 [Wadler] Variation two, revisited: State
//
//  behavior of "Monadic state eval"
//  it should "should count two divisions" in check {
//    forAll { (n: Int) =>
//      StateEvaluatorWithMonads.eval (answer).step (n) == (42,n+2) } }
//
//  it should "should count no divisions" in check {
//    forAll { (n: Int) =>
//      StateEvaluatorWithMonads.eval (const).step (n) == (42,n) } }
//
//  it should "throw a scala exception on division by 0" in
//  { intercept[java.lang.ArithmeticException] {
//  StateEvaluatorWithMonads.eval (error).step (0) } }
//
//  behavior of "State evaluators"
//
//  it should "behave identically (safe)" in check {
//    forAll (genSafeTerm) ( (t: Term) =>
//        forAll { (n: Int) =>
//      StateEvaluator.eval (t).step(n) ==
//        StateEvaluatorWithMonads.eval(t).step (n)
//     })
//  }
//
//
//  // Section 2.9 [Wadler] Output evaluator
//
//  behavior of "Monadic output eval"
//  it should "give good result and string output" in {
//    val r = OutputEvaluatorWithMonads.eval(answer)
//    r.a shouldBe 42
//    r.o shouldBe result
//  }
//
//  it should "return simple result for a constant" in {
//    val r = OutputEvaluatorWithMonads.eval(const)
//    r.a shouldBe 42
//    r.o shouldBe "eval(Cons(42)) <= 42\n"
//  }
//  it should "throw a scala exception on division by 0" in
//  { intercept[java.lang.ArithmeticException] {
//    OutputEvaluatorWithMonads.eval (error) } }
//
//
//  behavior of "Output evaluators"
//
//  private def repackage (r :OutputEvaluatorWithMonads.M[Int])
//      : OutputEvaluator.M[Int] =
//    r match { case OutputEvaluatorWithMonads.M(o,a) => OutputEvaluator.M(o,a) }
//
//  it should "behave identically (safe)" in check {
//    forAll (genSafeTerm) ( (t: Term) =>
//      OutputEvaluator.eval (t) == repackage(OutputEvaluatorWithMonads.eval(t))
//    )
//  }


  // AW TODO: look into monadic tests in Wadler's paper (Section 3)
  // AW TODO: refactor for DRY

}
