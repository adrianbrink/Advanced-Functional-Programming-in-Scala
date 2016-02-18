import scala.annotation.tailrec

sealed trait List[+A]
case object Nil extends List[Nothing]
case class Cons[+A](h: A, tail: List[A]) extends List[A]
object List {

  def ret[A,B] (as :List[A])(f: A => B) : B
  def sum(ints: List[Int]): Int = {
    ints match {
      case Nil => 0
      case Cons(x, xs) => x + sum(xs)
    }
  }

  def fact(prod: List[Int]): Int = {
    prod match {
      case Nil => 1
      case Cons(0.0, _) => 0
      case Cons(x, xs) => x * fact(xs)
    }
  }

  def tail[A](l: List[A]): List[A] = l match {
    case Nil => Nil
    case Cons(x, Cons(y, xs)) => Cons(y, xs)
  }


  def setHead[A](h: A, l: List[A]): List[A] = l match {
    case Nil => Cons(h, Nil)
    case Cons(_, xs) => Cons(h, xs)
  }

  def drop[A](l: List[A], n: Int): List[A] = {
    if(n == 0) l
    else {
      l match {
        case Nil => Nil
        case Cons(x, Cons(y, ys)) => drop(Cons(y, ys), n - 1)
      }
    }
  }

  def dropWhile[A](l: List[A])(f: A => Boolean): List[A] =
    l match {
      case Nil => l
      case Cons(x, Cons(y, ys)) => {
        if(f(x)) dropWhile(Cons(y, ys))(f)
        else Cons(x, dropWhile(Cons(y, ys))(f))
      }
      case _ => l
    }

  def init[A](l: List[A]): List[A] = l match {
    case Nil => Nil
    case Cons(x, xs) => Cons(x, init(xs))


  }

  def foldRight[A, B](as: List[A], z: B)(f: (A, B) => B): B = as match {
    case Nil => z
    case Cons(x, xs) => f(x, foldRight(xs, z)(f))
  }

  def product1(ns: List[Double]): Double = {
    foldRight(ns, 1.0)((x, y) => if(x != 0 && y != 0) x * y else 0.0)
  }
  def sum1(ns: List[Int]): Int ={
    foldRight(ns,1)(_+_)
  }

  def length1[A](as: List[A]): Int = foldRight(as, 0)((x, y) => y + 1)

  def foldLeft[A,B] (as: List[A], z: B)(f: (B,A) => B) : B = as match {
    case Nil => z
    case Cons(x, xs) => foldLeft(xs, f(z,x))(f)
  }

  def product2(ns: List[Double]): Double = {
    foldRight(ns, 1.0)((x, y) => if(x != 0 && y != 0) x * y else 0.0)
  }
  def sum2(ns: List[Int]): Int ={
    foldRight(ns,1)(_+_)
  }

  def length2[A](as: List[A]): Int = foldLeft(as, 0)((x, y) => x + 1)

  def reverse[A] (l: List[A]) : List[A] = {
      @tailrec
    def reverseHelp[A](l2: List[A], l1: List[A]): List[A] = {
      l2 match {
        case Nil => l1
        case Cons(x, ys) => reverseHelp(ys, Cons(x, l1))
      }
    }
    reverseHelp(l,Nil)
  }

  def foldLeft313[A,B] (as: List[A], z: B)(f: (B,A) => B) : B =  {
     foldRight(as,z)((a:A,b:B) => f(b,a))
  }

  def foldRight313[A, B](as: List[A], z: B)(f: (A, B) => B): B ={
    foldLeft(as,z)((b:B, a:A)=>f(a,b))
  }

  def append[A](a1: List[A], a2: List[A]): List[A]={
    foldLeft(reverse(a1),a2)((a2, a:A)=> Cons(a,a2))
  }

  def ex316(as: List[Int]): List[Int]= {
    @tailrec
    def help(xs: List[Int], rs: List[Int]): List[Int] = xs match {
      case Nil => reverse(rs)
      case Cons(x,cs) => help(cs, Cons(x+1, rs))
    }
    help(as,Nil)
  }

  def ex317(as: List[Double]): List[String]= {
    @tailrec
    def help(xs: List[Double], rs: List[String]): List[String] = xs match {
      case Nil => reverse(rs)
      case Cons(x,cs) => help(cs, Cons(x.toString, rs))
    }
    help(as,Nil)
  }

  def map[A,B](as: List[A])(f: A => B) : List[B] = {
    @tailrec
    def mapHelp[A,B](bs: List[A], cs : List[B])(f: A => B) : List[B] = bs match {
      case Nil =>  reverse(cs)
      case Cons(x,ds) => mapHelp(ds,(Cons(f(x),cs)))(f)
    }
    mapHelp(as, Nil)(f)
  }

  def filter[A](as: List[A])(f: A => Boolean) : List[A] = as match {
    case Nil => Nil
    case Cons(x,xs ) if(f(x)) =>  Cons(x, filter(xs)(f))
    case Cons(x,xs) if(!f(x)) => filter(xs)(f)
  }

  def flatMap[A,B](as: List[A])(f: A=> List[B]): List[B] = {
      @tailrec
    def flatMapHelp[A,B] (bs: List[A], cs: List[B])(f: A =>List[B]) : List[B]= bs match {
      case Nil => cs
      case Cons(x, xs) => flatMapHelp(xs, append(cs,f(x)))(f)
    }
  flatMapHelp(as,Nil)(f)
  }

  //def flatMap321[A,B](as: List[A])(f: A=> List[B]): List[A] = {
    //filter(as)((a:A)=>f(a) == as)
  //}


  def add(as : List[Int], bs: List[Int]): List[Int] = {
    @tailrec
    def addHelp(cs: List[Int], ds: List[Int], es: List[Int] ): List[Int] = cs match {
      case Nil => ds match {
        case Nil => reverse(es)
        case Cons(x,xs) => addHelp(cs,xs,Cons(x,es))
      }
      case Cons(y,ys) => ds match {
        case Nil => addHelp(ys,ds,Cons(y,es))
        case Cons(f, fs) => addHelp(ys, fs,Cons(f+y, es))
      }
    }
    addHelp(as,bs,Nil)
  }

  def zipWith[A](as : List[A], bs: List[A])(f:(A,A) => A): List[A] = {
    @tailrec
    def zipWithHelp[A](cs: List[A], ds: List[A], es: List[A] )(f:(A,A) => A): List[A] = cs match {
      case Nil => ds match {
        case Nil => reverse(es)
        case Cons(x,xs) => zipWithHelp(cs,xs,Cons(x,es))(f)
      }
      case Cons(y,ys) => ds match {
        case Nil => zipWithHelp(ys,ds,Cons(y,es))(f)
        case Cons(g, fs) => zipWithHelp(ys, fs,Cons(f(g,y), es))(f)
      }
    }
    zipWithHelp(as,bs,Nil)(f)
  }

  def hasSubsequence[A](sup: List[A], sub: List[A]): Boolean = {
    @tailrec
    def hasSubsequenceHelp[A](sup1: List[A], sub1: List[A], bool: Boolean): Boolean = sup1 match {
      case  Nil => bool
      case Cons(x,xs) => sub1 match {
        case Nil => bool
        case Cons(y,ys) if(x==y) =>first(xs,ys,(x==y))
        case Cons(y,ys) => hasSubsequenceHelp(xs,sub1,false)
      }
    }
    def first[A](sup2:List[A],sub2: List[A], bool1: Boolean ) : Boolean = {
      sup2 match {
        case Nil => bool1
        case Cons(x, xs) => sub2 match {
          case Nil => bool1
          case Cons(y, ys) if (y == x) => first(xs, ys, x == y && bool1)
          case Cons(y, ys) => first(xs, ys, false)
        }
      }
    }
    hasSubsequenceHelp(sup,sub,false)
  }

  def apply[A](as: A*): List[A] = {
    if(as.isEmpty) Nil
    else Cons(as.head, apply(as.tail: _*))
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