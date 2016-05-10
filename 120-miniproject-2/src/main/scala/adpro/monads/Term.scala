/* ADVANCED PROGRAMMING. MINIPROJECT 2. Monads
   Andrzej Wąsowski */

package adpro.monads
import scala.language.higherKinds

// Work through this file top down

// Section 2.1 [Wadler]

// Wadler uses a langauge similar to Haskell to implement his evaluator. We will
// use scala.  This is Wadler's Term language implemented in Scala:

sealed trait Term
case class Cons (value :Int) extends Term
case class Div (left :Term, right :Term) extends Term


// From now on we create one module (object) per section to avoid name clashes
// between different variants.

// This is the basic evaluator (compare to the paper, to see whether you
// understand)

object BasicEvaluator {
  def eval (term :Term) :Int = term match {
    case Cons(a) => a
    case Div(l, r) => eval(l) / eval(r)
  }
}
 object ExceptionEvaluator {

   type Exception = String
   trait M[+A]
   case class Raise (e: String) extends M[Nothing]
   case class Return[A] (a: A) extends M[A]
   
   def eval (term :Term) :M[Int] = term match {
     case Eval(a) =>  Return (a)
     case _ => Raise("Division by zero")
   }
   //Extractor
   object Eval{
     def unapply(t: Term): Option[Int] = t match {
       case Cons(a) => Some(a)
       case Div(l, r) => unapply(l).flatMap(a => unapply(r).flatMap(b => if(b != 0)Some(a /b) else None))
       case _ => None
     }
   }

 }

// // Section 2.3 [Wadler] Variation two: State
//
 object StateEvaluator {

   type State = Int
   case class M[+A] (step: State => (A,State))

   def eval (term :Term) :M[Int] = term match {
     case Cons (a) => M[Int] (x => (a,x))
     case Div (t,u) => M[Int]( s =>{ 
       val (a, st) = eval(t).step(s)
       val (b, st1) = eval(u).step(st)
       ((a / b), st1 + 1)
     })        
   }

}

 object OutputEvaluator {

   type Output = String
   case class M[+A] (o: Output, a: A)

   def line (a :Term) (v :Int) :Output =
     "eval(" + a.toString + ") <= " + v.toString + "\n"

   def eval (term :Term) :M[Int] = term match {
     case Cons (a) => M[Int](line(term)(a), a) 
     case Div (l, r) => {
       val x = eval(l)
       val y = eval(r)
       M[Int] ((x.o + y.o + line(term)(x.a / y.a)), x.a/y.a)
     }
   }
 }

// Section 2.5 [Wadler] A monadic evaluator
// // The following are two generic monadic interfaces (one for classes, one for
// // meta-classes/objects) that we will use to type check our monadic solutions.
// // We shall provide flatMap and map for our monads to be able to use for
// // comprehensions in Scala.
// // IMPORTANT: flatMap is called "(*)" in the paper.
//
 trait Monad[+A,M[_]] {
   def flatMap[B] (k: A => M[B]) :M[B]
   def map[B] (k: A => B) :M[B]
 }
//
// // we will provide unit, as the paper does. This will be placed in a companion
// // object.
 trait MonadOps[M[_]] { 
   def unit [A] (a :A) :M[A] 
 }
// // The above abstract traits will be used to constraint types of all our monadic
// // implementations, just to ensure better type safety and uniform interfaces.
// // Now we are startin to implement the monadic evaluator from the paper.
// // Compare this implementation to the paper, and make sure that you understand
// // the Scala rendering.
//
// // Section 2.6 [Wadler] Variation zero, revisited: The basic evaluator
//
 object BasicEvaluatorWithMonads {
//
//    We enrich our M type with flatMap and map;
//   // A flatMap is already in the paper (called *)
//   // I add map, so that we can use for comprehensions with this type
   case class M[+A] (a: A) extends Monad[A,M] {
     def flatMap[B] (k: A => M[B]) :M[B] = k (this.a)
     def map[B] (k: A => B) :M[B] = flatMap[B] (a => M.unit(k(a))) 
   }

   object M extends MonadOps[M] { def unit[A] (a : A) :M[A] = M[A] (a) }

   def eval (term: Term) :M[Int] = term match {
     case Cons (a) => M.unit (a)
     case Div (t,u) => for {
       a <- eval (t)
       b <- eval (u)
       r <- M.unit (a/b)
     } yield r
   }
// TODO: Make sure that you understand the above implementation (an dhow it
//   // relates to the one in the paper). If you find the for comprehension to be
//   // obscuring things, you may want to rewrite the above using just map and
//   // flatMap.
}

// Section 2.7 [Wadler] The monadic evaluator with exceptions
//
 object ExceptionEvaluatorWithMonads {

   type Exception = String

   trait M[+A] extends Monad[A,M]{

     def flatMap[B] (k: A => M[B]) :M[B] = this match {
        case Raise (e) => Raise (e)
        case Return (a) => k(a)
     }

     def map[B] (k: A => B) :M[B] = flatMap(a => M.unit(k(a)))
   }

   object M extends MonadOps[M] { 
     def unit[A] (a : A) :M[A] = Return (a) 
   }

   case class Raise (e: String) extends M[Nothing]
   case class Return[A] (a: A) extends M[A]

//   // TODO: complete the evaluator
   def eval (term :Term) :M[Int] = term match {
     case Cons (a) => M.unit (a)
     case Div (t,u) => for {
       a <- eval(t)
       b <- eval(u)
       r <- if(b == 0) raise( "Divided by zero") else M.unit(a/b)
     } yield r
   }

   def raise(ex: Exception): M[Nothing] = Raise(ex)
//
//   // TODO: Discuss in the group how the monadic evaluator with exceptions
//   // differs from the monadic basic one
 }

// Section 2.8 [Wadler] Variation two, revisited: State

object StateEvaluatorWithMonads {

   type State = Int

   case class M[+A] (step: State => (A,State)) extends Monad[A,M] {

     // flatMap is bind or (*) in the paper
     def flatMap[B] (k :A => M[B]) = M[B] {
       x => { 
         val (a,y) = step (x)
         k(a).step(y) 
       } 
     }
     def map[B] (k :A => B) :M[B] = flatMap (a => M.unit(k(a)))
   }

  //  TODO: complete the implementation of unit, based on the paper
   object M extends MonadOps[M] { 
     def unit[A] (a : A) :M[A] = M[A] (s => (a, s))  
   }

   // TODO: complete the implementation of the evalutor:
   def eval (term :Term) :M[State] = term match {
     case Cons (a) => M.unit (a)
     case Div (t,u) => for {
       a <- eval(t)
       b <- eval(u)
       r <- M[State](s => (a / b, s + 1)) 
     }  yield r
   }

   // TODO: Discuss in the group how the monadic evaluator with counter differs
   // from the monadic basic one (or the one with exceptions)
}

 // Section 2.9 [Wadler] Output evaluator

 object OutputEvaluatorWithMonads {

   type Output = String

   case class M[+A] (o: Output, a: A)  extends Monad[A, M]{

     // flatMap is (*) in [Wadler]
     // TODO: implement flatMap
     def flatMap[B] (k :A => M[B]) :M[B] = M(this.o ++ k(this.a).o, k(a).a)
     def map[B] (k :A => B) :M[B] = M[B] (this.o, k(this.a))

   }

   // TODO: implement unit
 object M extends MonadOps[M] { def unit[A] (a : A) :M[A] = M[A] ("", a) }

   def line (a :Term) (v :Int) :Output =
     "eval(" + a.toString + ") <= " + v.toString + "\n"

//   // TODO: implement eval
   def eval (term :Term) :M[Int] = term match{
     case Cons (a) => out(line(term)(a)) map ( _ => a ) 
     case Div (t, u) => for {
       a <- eval(t)
       b <- eval(u)
       r <-  out(line(term)(a/b)) map ( e => a / b)
     } yield r 
   }
   def out(o: Output): M[Any] = M(o,()) 

//   // Discuss in the group how the monadic evaluator with output differs from
//   // the monadic basic one (or the one with state/counter).
 }

// Answer Question II below


