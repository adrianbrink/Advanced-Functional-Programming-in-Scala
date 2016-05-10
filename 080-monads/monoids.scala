trait Monoid[A]{
    def op(x1: A, x2: A): A
    def zero : A
}
// Exercise 10.1
val intAddition: Monoid[Int] = new Monoid[Int]{
  def op(x1: Int, x2: Int): Int = x1 + x2
  val zero  = 0 
}
val intMultiplication: Monoid[Int] = new Monoid[Int] {
  def op(x1: Int, x2: Int): Int = x1 * x2
  val zero = 1
}
val booleanOr: Monoid[Boolean] = new Monoid[Boolean]{
  def op(x1: Boolean, x2: Boolean): Boolean = x1 || x2
  val zero = true
}
val booleanAnd: Monoid[Boolean] = new Monoid[Boolean]{
  def op(x1: Boolean, x2: Boolean): Boolean = x1 && x2
  val zero = true
}
def optionMonoid[A] : Monoid[Option[A]] = new Monoid[Option[A]]{
 def op(x1: Option[A], x2: Option[A]) = x1 orElse x2
 def zero = None
}
//Exercise 10.3
def endoMonoid[A]: Monoid[A => A] = new Monoid[A => A] {
  val op(x1: A => A, x2: A => A): Monoid[A => A] = x1 compose x2
  val zero = a => a
} 
// Exercise 10.5
 def foldMap[A,B](as: List[A], Monoid[B])(f: A => B) :B ={
   (as map f).foldLeft(m.zero)(m.op) 
}
// Exercise 10.6 
def foldLeft[A,B](as: List[A], z: => B)(f: (A,B) => B) : B = {
   foldMap[A,B] (as, val mon =  new Monoid[B]{
           val op = f
           val zero = z
           mon})(f(_: A, z))
}
// Exercise 10.7
def foldMapV[A, B](v: IndexedSeq[A], m: Monoid[B])(f: A => B) : B ={
   val seq = v map f
   def foldBalanced[B] (as: IndexedSeq[B], z: => B)(g: (B, B) => B): B ={
                 if(as.length == 1) as(0)
                 else if(as.length == 2) g(as(0), as(1))
                 else if(as.isEmpty) z
                 else {
                     val  (seq1, seq2) = as.splitAt(as.length/2)
                     g (foldBalanced( seq1, z)(g), foldBalanced( seq2, z)(g))
                 }
}
  foldBalanced(seq, m.zero)(m.op)
}  
// Exercise 10.9
//def isSorted(s: indexedSeq[Int]) =....

sealed trait WC 
case class Stub(chars: String) extends WC
case class Part(lStub: String, words: Int, rStub: String) extends WC

// Exercise 10.10
val wcMonoid: Monoid[WC] = new Monoid[WC]{
  def op(x1: WC, x2: WC) = x1 match{
                             case Stub(ch) => x2 match {
                                                    case Stub(ch1) => Stub(ch1 + ch)
                                                    case Part(l, w, r) if(l == "") => Part(c + l, w, r)
                                                    case Part(l, w, r) =>    Part(l, w, r + ch)
                                               } 
                             case Part(l, w, r)  => x2 match{
                                                    case Stub(ch) if(l == "") => Part(ch + l, w, r)
                                                    case Stub(ch) if(r == "") => Part( l, w, ch + r)
                                                    case Part(l1, w1, r1) => Part(l1 + l2, w + w1, r1 + r2)
                                                  }
                              }

  def zero = Stub("")
}

trait Foldable[F[_]] {
def foldRight[A, B] ( as: F[A])(z: => B)(f: (A, B) => B): B
def foldLeft[A, B]( as: F[A])(z: => B)(f: (B, A) => B): B
def foldMap[A, B](as: F[A])(f: A => B) (mb: Monoid[B]): B
def concatenate[A](as: F[A]) (m: Monoid[A]): A = foldLeft(as)(m.zero)(m.op)
// Exercise 10.14
def toList[A](fa: F[A]): List[A] =  foldRight(fa)(Nil)((a, b) => a :: b)
}
// Exercise 10.12
val foldable: Foldable[List] = new Foldable[List]{
   def foldRight[A, B](as: List[A])(z: => B)(f: (A, B) => B): B = {
         as match {
                case Nil => z
                case h :: t => f(h, foldRight(t)(z)(f)) 
         }
   }
   def foldLeft[A, B](as: List[A])(z: => B)(f: (B, A) => B): B = {
         as match {
                case Nil => z
                case h :: t => f(foldLeft(t)(z)(f), h) 
         }
   }
def foldMap[A, B] (as: List[A])(f: A => B)(mb: Monoid[B]): B ={
   val l = as map f 
     foldLeft(l)(mb.zero)(mb.op)
 }
}

val foldableSeq: Foldable[IndexedSeq] = new Foldable[IndexedSeq]{
   def foldRight[A, B](as: IndexedSeq[A])(z: => B)(f: (A, B) => B): B = {
         if(as.length == 0 ) z
         else if(as.length == 1) f(as(0), z)
         else {
         val (v1, v2) = as.splitAt(as.length - 2)
         f(v2, foldRight(v1)(z)(f))
         }
   }
   def foldLeft[A, B](as: IndexedSeq[A])(z: => B)(f: (B, A) => B): B = {
         if(as.length == 0 ) z
         else if(as.length == 1) f(z ,as(0))
         else {
         val (v1, v2) = as.splitAt(as.length - 2)
         f(foldRight(v1)(z)(f), v2)
         }
   }
def foldMap[A, B] (as: IndexedSeq[A])(f: A => B)(mb: Monoid[B]): B ={
   val l = as map f 
     foldLeft(l)(mb.zero)(mb.op)
 }
}
val foldableStream: Foldable[Stream] = new Foldable[Stream]{
   def foldRight[A, B](as: Stream[A])(z: => B)(f: (A, B) => B): B = {
         as match {
                case Empty => z
                case Cons(h, t) => f(h(), foldRight(t)(z)(f)) 
         }
   }
   def foldLeft[A, B](as: Stream[A])(z: => B)(f: (B, A) => B): B = {
         as match {
                case Nil => z
                case Cons(h, t) => f(foldLeft(t)(z)(f), h()) 
         }
   }
def foldMap[A, B] (as: Stream[A])(f: A => B)(mb: Monoid[B]): B ={
   val l = as map f 
     foldLeft(l)(mb.zero)(mb.op)
 }
}   

// Exercise 10.13
val foldableTree: Foldable[Tree] = new Foldable[Tree]{
   def foldRight[A, B](as: Tree[A])(z: => B)(f: (A, B) => B): B ={
      as match {
           case Branch(Leaf(v) , r) => f(v, foldRight(r)(z)(f))
           case Branch(l, Leaf(v)) =>  f(l, foldRight(r)(z)(f))
           case Branch(l, r) =>        foldRight(l)(z)(f)
           case Leaf(v) =>             f(v, z)
     }
  }
  def foldLeft[A, B](as: Tree[A])(z: => B)(f: (A, B) => B): B ={
      as match {
           case Branch(Leaf(v) , r) => f(foldLeft(r)(z)(f), v)
           case Branch(l, Leaf(v)) =>  f(foldLeft(r)(z)(f), l)
           case Branch(l, r) =>        foldLeft(l)(z)(f)
           case Leaf(v) =>             f(v, z)
     }
  }
  def foldMap[A, B] (as: Stream[A])(f: A => B)(mb: Monoid[B]): B ={
   val l = as map f 
     foldLeft(l)(mb.zero)(mb.op)
  }
}
// Exercise 10.14
val foldableOption: Foldable[Option] = new Foldable[Option]{
 def foldRight(as: Option[A])(z: => B)(f: (A, B) => B) = as match {
                   case Some(a) => Some(f(a, z))
                   case None => z
              }
def foldLeft(as: Option[A])(z: => B)(f: (B, A) => B) = as match {
                   case Some(a) => Some(f(z, a))
                   case None => z
              } 
 def foldMap[A, B] (as: Stream[A])(f: A => B)(mb: Monoid[B]): B ={
   val l = as map f 
     foldLeft(l)(mb.zero)(mb.op)
  }
}
// Exercise 10.16
def productMonoid[A, B](a: Monoid[A], b: Monoid[B]): Monoid[(A, b)] = 
   new Monoid[(A, B)]{ 
    def op( x1: (A, B), x2: (A, B)) = (a.op(x1._1, x2._ 2), b.op(x1._2, x2._2))
    def zero = (a.zero, b.zero)
}
// Exercise 10.18
def funtionMonoid[A, B](b: Monoid[B]): Monoid[A => B] = new Monoid[A => B]{
def op(f: A => B, g: A => B) : A => B = a => b.op(f(a), g(a))
def zero = a => b.zero
}
// Exercise 10.19
def bag[A](as: IndexedSeq[A]): Map[A, Int] =
as.foldLeft(Map[A, Int]())((map, a => mapMergeMonoid[A, Int](intAddition).op(map, Map(a -> 1)))


