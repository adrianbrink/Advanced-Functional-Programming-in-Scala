import java.util.concurrent._
import scala.language.implicitConversions

// Work through the file top-down, following the exercises from the week's
// sheet.  Uncomment and complete code fragments.

object Par {

  type Par[A] = ExecutorService => Future[A]
  def run[A] (s: ExecutorService) (a: Par[A]) : Future[A] = a(s)


  case class UnitFuture[A] (get: A) extends Future[A] {
    def isDone = true
    def get (timeout: Long, units: TimeUnit) = get
    def isCancelled = false
    def cancel (evenIfRunning: Boolean) : Boolean = false
  }

  def unit[A] (a: A) :Par[A] = (es: ExecutorService) => UnitFuture(a)

  def map2[A,B,C] (a: Par[A], b: Par[B]) (f: (A,B) => C) : Par[C] =
    (es: ExecutorService) => {
      val af = a (es)
      val bf = b (es)
      UnitFuture (f(af.get, bf.get))
    }

  def fork[A] (a: => Par[A]) : Par[A] = es => es.submit(
    new Callable[A] { def call = a(es).get }
  )

  def lazyUnit[A] (a: =>A) : Par[A] = fork(unit(a))

  // Exercise 1 (CB7.4) (8 minutes)

  // def asyncF[A,B] (f: A => B) : A => Par[B] =

  // map is shown in the book

  def map[A,B] (pa: Par[A]) (f: A => B) : Par[B] =
    map2 (pa,unit (())) ((a,_) => f(a))

  // Exercise 2 (CB7.5)

  // def sequence[A] (ps: List[Par[A]]): Par[List[A]] = ...

  // Exercise 3 (CB7.6)

  // this is shown in the book:

  // def parMap[A,B](ps: List[A])(f: A => B): Par[List[B]] = fork {
  //   val fbs: List[Par[B]] = ps.map(asyncF(f))
  //   sequence(fbs)
  // }

  // def parFilter[A](as: List[A])(f: A => Boolean): Par[List[A]] = ...

  // Exercise 4: implement map3 using map2 (5 minutes)

  // def map3[A,B,C,D] (pa :Par[A], pb: Par[B], pc: Par[C]) (f: (A,B,C) => D) = ...

  // shown in the book

  // def equal[A](e: ExecutorService)(p: Par[A], p2: Par[A]): Boolean = p(e).get == p2(e).get

  // Exercise 5 (CB7.11) (10 minutes)

  // def choiceN[A] (n: Par[Int]) (choices: List[Par[A]]) :Par[A] =

  // def choice[A] (cond: Par[Boolean]) (t: Par[A], f: Par[A]) : Par[A] =

  // Exercise 6 (CB7.13) (10 minutes)

  // def chooser[A,B] (pa: Par[A]) (choices: A => Par[B]): Par[B] =

  // def choiceNviaChooser[A] (n: Par[Int]) (choices: List[Par[A]]) :Par[A] =

  // def choiceViaChooser[A] (cond: Par[Boolean]) (t: Par[A], f: Par[A]) : Par[A] =

  // Exercise 7 (CB7.14)

  // def join[A] (a : Par[Par[A]]) :Par[A] =

  class ParOps[A](p: Par[A]) {

  }

  implicit def toParOps[A](p: Par[A]): ParOps[A] = new ParOps(p)
}
