import  java.awt.{Point => P}
import scala.annotation.tailrec
// Exercise 1 using Class

class OrderedPoint() extends P with scala.math.Ordered[P]{

 def this(x: Int, y:Int){

      this()
      this.x = x
      this.y = y
 }
  override def compare(that: P): Int = {
    (x - that.x + y - that.y)
  }
}
// Exercise using Trait
trait Ordered extends P with scala.math.Ordered[P]{
  override def compare(that: P): Int = {
    (x - that.x + y - that.y)
  }
}

sealed trait Tree[+A]
case class Leaf[A](value: A) extends Tree[A]
case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]
object Tree{

  def size[A](as: Tree[A]) :Int = {
    def sizeHelp(bs: Tree[A], nu: Int) : Int = bs match {
      case Branch(l,r) => sizeHelp(l, 1 + nu) + sizeHelp(r, 1 + nu)
      case Leaf(_) => nu + 1
    }
    sizeHelp(as,0)
  }

  def maximum(as: Tree[Int]) : Int = {
    def max1(bs : Tree[Int], maxInt: Int): Int = bs match {
      case Branch(l,r) => max1(l,maxInt).max(max1(r,maxInt))
      case Leaf(value) => value
    }
    max1(as,-100000)
  }

  def depth[A](as: Tree[A]) : Int ={
    def dep[A](bs: Tree[A], lvl: Int): Int = bs match {
      case Branch(l,r) => dep(l,lvl+1).max(dep(r,lvl+1))
      case Leaf(_) => lvl
    }
    dep(as,0)
  }

  def map[A,B] (as: Tree[A])(f: A => B): Tree[B] = as match {
    case Branch(l,r) => Branch(map(l)(f),map(r)(f))
    case Leaf(value) => Leaf(f(value))
  }
}

case class some[+A] (get: A) extends Option[A]
case object None extends Option[Nothing]
sealed trait Option[+A]{

  def map[B] (f: A => B): Option[B]={
    this match {
      case None => None
      case some(get) => some(f(get))
    }
  }


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
object Option {

  def means(xs: Seq[Double]): Option[Double] = {
    if (xs.isEmpty) None
    else some(xs.sum / xs.length)
  }

  def map2[A, B, C](a: Option[A], b: Option[B])(f: (A, B) => C): Option[C] = {
    // a flatMap(aa => b flatMap(bb => some(f(aa,bb))))
    for (aa <- a;
         bb <- b)
      yield (f(aa, bb))
  }

  def variance(xs: Seq[Double]): Option[Double] = {
    means(xs) flatMap (m => means(xs map (x => math.pow(x - m, 2))))
  }

  def sequence[A](as: List[Option[A]]) : Option[List[A]]= {

  }
}

object Test extends App{
  val a = new OrderedPoint(1,2)
  val b = new OrderedPoint(3,4)
  println(a < b)
  val c = new P(12,3) with Ordered
  val d = new P(3,2) with Ordered
  println(d > c)

}