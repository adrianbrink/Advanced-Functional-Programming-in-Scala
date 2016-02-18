// Advanced Programming 2015
// Andrzej Wąsowski, IT University of Copenhagen
//
// An example comparing methods (f) and functions (g) in scala,
// with respect to dynamic and static call dispatch

class A {

  def f () = println ("A")
  val g = () => println ("A")
}

class B extends A {

  override def f () = println ("B")
  override val g = () => println ("B") // g does not overload A.g,
                                       // but hides it
}

object DynamicDispatchMain extends App {

  val a1 = new A()
  val b1 = new B()
  val a2 :A = b1 // upcast b1 to A

  a1.f()
  b1.f()
  a2.f() // prints B, oo-polymorphism
         // (dynamic dispatch), virtual call

  println

  a1.g()
  b1.g()
  a2.g()  // prints A, no oo-polymorphism
          // static dispatch
}
