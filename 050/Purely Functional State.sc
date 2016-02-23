

sealed trait RNG{
  def nextInt: (Int, RNG)
}
// Type alias for the RNG => (A, RNG)
type Rand[+A] = RNG => (A, RNG)
case class SimpleRng(seed: Long) extends RNG{
  def nextInt :(Int, RNG) = {
    val newSeed = (seed * 0x5DEECE66DL + 0xBL) & 0xFFFFFFFFFFFFL
    val nextRng = SimpleRng(newSeed)
    val n = (newSeed >>> 16).toInt
    (n,nextRng)
  }

}

object RNG {
  def nonNegativeInt(rng: RNG): (Int, RNG) ={
    rng.nextInt match {
      case (y, rn) if( y > 0 && y < Int.MaxValue) => (y, rn)
      case (y, rn) => nonNegativeInt(rn)
    }
  }
  //def double(rng: RNG): (Double, RNG) ={
    //val
  //}
  def intDouble(rng: RNG): ((Int,Double), RNG) ={
    ((rng.nextInt._1,rng.nextInt._2.nextInt._1.toDouble),rng.nextInt._2.nextInt._2)
  }


  def doubleInt(rng: RNG):((Double, Int),RNG) ={
    ((rng.nextInt._1.toDouble,rng.nextInt._2.nextInt._1),rng.nextInt._2.nextInt._2)
  }
  def double3(rng: RNG): ((Double,Double,Double), RNG) ={
    ((rng.nextInt._1.toDouble,rng.nextInt._2.nextInt._1.toDouble,rng.nextInt._2.nextInt._2.nextInt._1.toDouble),rng.nextInt._2.nextInt._2.nextInt._2)
  }
  def ints(count: Int)(rng: RNG): (collection.immutable.List[Int], RNG) ={
    def go(cnt: Int)(l1: collection.immutable.List[Int], rn: RNG): (collection.immutable.List[Int], RNG) ={
      if(cnt == 0) (l1,rn)
      else   go(cnt - 1)(rn.nextInt._1 :: l1,rn.nextInt._2 )
    }
    go(count)(collection.immutable.Nil,rng)
  }
  def map[A,B](as: Rand[A])(f: A =>B): Rand[B] = {
    rand => as(rand) match {
      case (x, y) => (f(x), y)
    }
  }
  def unit[A](a: A): Rand[A] = rng => (a,rng)
  def map2[A,B,C](ra: Rand[A], rb: Rand[B])(f: (A, B) => C): Rand[C] ={
     rand => ra(rand) match {
       case (x, y) => rb(rand) match {
         case (x1, y1) => (f(x,x1),rand)
       }
     }
  }
  def double(rng: RNG): Rand[Double] = {
    map(unit(nonNegativeInt(rng)._1.toDouble))(i => i - (i.toInt) )
  }
}