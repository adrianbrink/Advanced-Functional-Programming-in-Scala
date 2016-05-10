package fpinscala.monads
import scala.language.higherKinds

trait Functor[F[_]] {

  def map[A,B] (fa: F[A]) (f: A => B) :F[B]

  def distribute[A,B] (fab: F[(A,B)]): (F[A],F[B]) =
    (map (fab) (_._1), map (fab)(_._2))

  def codistribute[A,B] (e :Either[F[A],F[B]]): F[Either[A,B]] = e match {
    case Left(fa) => map (fa) (Left(_))
    case Right(fb) => map (fb) (Right(_))
  }
}

object Functor {

  val ListFunctor = new Functor[List] {
    def map[A,B] (as: List[A]) (f: A => B): List[B] = as.map (f)
  }

     
}

trait Monad[F[_]] extends Functor[F] {

  def unit[A]  (a: => A): F[A]
  def flatMap[A,B] (ma: F[A]) (f: A => F[B]) :F[B]

  def map[A,B] (ma: F[A]) (f: A => B) :F[B] =
    flatMap (ma) (a => unit (f(a)))

  def map2[A, B, C] (ma: F[A], mb: F[B]) (f: (A,B) => C): F[C] =
    flatMap (ma) (a => map (mb) (b => f(a,b)))
 
  def product[A, B] (ma: F[A], mb: F[B]) : F[(A, B)] = map2(ma, mb)((_, _))
  def filterM[A](ms: List[A])(f: A => F[Boolean]): F[List[A]] ={
           ms.foldRigth(unit(List(Nil): F[List[A]]))((a, b) => flatMap(f(a))(c => if(c)unit(a :: b)
                                                                                    else unit(List(b))))
  }

  // Exercise 11.3

    def sequence[A] (lfa: List[F[A]]): F[List[A]] =    
     lfa match{
         case h :: t => flatMap(h)(a => unit(a :: sequence(t)(f)))
         case Nil =>  unit(Nil)
     }

  // traverse seems to simply sequence results of mapping.  I do not think that
  // it appeared in our part. You can uncomment it once you have sequence.
    def traverse[A,B] (la: List[A]) (f: A => F[B]): F[List[B]] = sequence(la.map (f))

  // Exercise 11.4

  // def replicateM[A] (n: Int, ma: F[A]): F[List[A]] ={ 
           val lfa: List[F[A]] =  List.fill(n)(ma)
           sequence(lfa)
    } 
 

  def join[A] (mma: F[F[A]]): F[A] = flatMap (mma) (ma => ma)


 
  // Exercise 11.7
 
  def compose[A, B, C](f: A => F[B], g: B => F[C]): A => F[C] = a => f(a).flatMap(b => g(b))

  // Exercise 11.8

   def flatMap_compose[A,B] (ma: F[A]) (f: A => F[B]) :F[B] = compose(() => ma, f)()  

}

object Monad {

  // Exercise 11.1

  // val optionMonad = new Monad[Option]{
     def unit[A](a: => A) : Option[A] = Some(a)
     def flatMap[A, B]( ma: Option[A])(f: A => Option[B]): Option[B]  = ma match{
          case Some(x) => f(x)
          case None =>    None: Option[B]
         }
     } 

   val streamMonad = new Monad[Stream]{
     def unit[A](a: => A): Stream[A] = Stream.unit(a)
     def flatMap[A](ma: Stream[A])(f: A => Stream[B]): Stream[B] = ma match{
         case Cons(h, t) => Stream.cons(f(h), flatMap(t())(f)) 
         case Empty => Stream.empty
     }
    }

   val ListMonad = new Monad[List]{
     def unit[A] (a: => A) : List[A] = List(a)
     def flatMap[A, B] (ma: List[A])( f: A =>List[B]): List[B] = 
         ma match {
              case h :: t => f(a) append flatMap(t)(f)
              case Nil =>  Nil 
          }
   }


}


