import scala.annotation.tailrec

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


case class Left[+E](value: E) extends Either[E, Nothing]
case class Right[+A] (value: A) extends Either[Nothing, A]

sealed trait Either[+E, +A]{
  def Try[A](a: => A) : Either[Exception,A]={
    try Right(a)
    catch{case e : Exception => Left(e)}
  }

  def map[B](f: A => B): Either[E, B] ={
    this match {
      case Right(a) => Right(f(a))
      case Left(e) => Left(e)
    }
  }

  def flatMap[EE >: E, B](f: A => Either[EE, B]) : Either[EE, B]={
    this match {
      case Right(a) => f(a)
      case Left(e) => Left(e)
    }
  }

  def  orElse[EE >: E, B] (b: => Either[EE, B]): Either[EE, B] ={
    this match {
      case Left(e) => Left(e)
      case Right(a) =>  b
    }
  }

  def map2[EE >: E, B, C] (b: Either[EE, B])(f: (A, B) => C) : Either[EE, C] = {
    //this flatMap(aa => b map(bb => f(aa,bb)))
    // for-comprehension
    for { aa <- this
          bb <- b
    }yield f(aa,bb)
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


