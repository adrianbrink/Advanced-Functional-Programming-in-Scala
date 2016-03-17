
package exercises.laziness {

  case class Some[+A](get: A) extends Option[A]
  case object None extends Option[Nothing]
  sealed trait Option[+A] {
    def map[B](f: A => B): Option[B] = {
      this match {
        case None => None
        case Some(get) => Some(f(get))
      }
    }


    def flatMap[B](f: A => Option[B]): Option[B] = {
      this match {
        case Some(x) => f(x)
        case None => None
      }
    }

    def getOrElse[B >: A](ob: => B): B = {
      this match {
        case None => ob
        case Some(x) => x
      }
    }

    def orElse[B >: A](ob: => Option[B]): Option[B] = {
      map(x => Some(x)).getOrElse(ob)
    }

    def filter(f: A => Boolean): Option[A] = {

      flatMap(x => if (f(x)) Some(x) else None)
    }
  }


  case object Empty extends Stream[Nothing]

  case class Cons[+A](h: () => A, t: () => Stream[A]) extends Stream[A]

  object Stream {

    def cons[A](h: => A, t: => Stream[A]) = {
      lazy val head = h
      lazy val tail = t
      Cons(() => head, () => tail)
    }

    def map[A, B](as: Stream[A])(f: A => B): Stream[B] = {
      unfold(as)(x => x match {
        case Cons(h, t) => Some((f(h()), t()))
        case Empty => None
      })
    }

    def take[A](as: Stream[A], n: Int): Stream[A] = {
      unfold((as, n))(x => x match {
        case (Empty, _) => None
        case (Cons(h, t), 0) => None
        case (Cons(h, t), y) => Some(h(), (t(), y - 1))
      })
    }

    def constant[A](a: A): Stream[A] = cons(a, constant(a))

    def unfold[A, S](z: S)(f: S => Option[(A, S)]): Stream[A] = {
      f(z) match {
        case Some((a, b)) => cons(a, unfold(b)(f))
        case none => empty
      }
    }

    def from(n: Int): Stream[Int] = cons(n, from(n + 1))

    def fibs: Stream[Int] = {
      def help(n0: Int, n1: Int): Stream[Int] = {
        cons(n0 + n1, help(n1, n0 + n1))
      }
      help(0, 1)
    }

    def fibsUnfold: Stream[Int] = {
      unfold((0, 1))((x) => Some(x._1 + x._2, (x._2, x._1 + x._2)))
    }

    def fromUnfold(n: Int): Stream[Int] = {
      unfold(n)((x) => Some(x, x + 1))
    }

    def constantUnfold[A](a: A): Stream[A] = {
      unfold(a)((x) => Some(x, x))
    }

    def onesUnfold: Stream[Int] = unfold(1)((x) => Some(1, 1))

    def empty[A]: Stream[A] = Empty

    def apply[A](as: A*): Stream[A] = {
      if (as.isEmpty) empty
      else cons(as.head, apply(as.tail: _*))
    }

    def takeWhile[A](as: Stream[A])(p: A => Boolean): Stream[A] = {
      unfold(as)(x => x match {
        case Cons(h, t) if (p(h())) => Some((h(), t()))
        case Empty => None
        //case Cons(h,Cons(h1,t)) => Some()
      })
    }

    def startsWith[A](as: Stream[A], s: Stream[A]): Boolean = {
      unfold((as, s))(x => x match {
        case (Cons(h, t), Cons(h1, t1)) => Some((h(), h1()), (t(), t1()))
        case _ => None
      }).filter(y => y._1.equals(y._2)).forAll(z => z !=(null, null))
    }

    def zipWith[A, B](as: Stream[A], bs: Stream[B])(f: (A, B) => B): Stream[B] = {
      unfold((as, bs))(x => x match {
        case (Cons(h, t), Cons(h1, t1)) => Some(f(h(), h1()), (t(), t1()))
        case _ => None
      })
    }

    def zipAll[A, B](as: Stream[A], bs: Stream[B]): Stream[(Option[A], Option[B])] = {
      unfold((as, bs))(x => x match {
        case ((Cons(h, t), Cons(h1, t1))) => Some((Some(h()), Some(h1())), ((t(), t1())))
        case ((Empty, Cons(h, t))) => Some((None, Some(h())), (Empty, t()))
        case ((Cons(h, t)), Empty) => Some(((Some(h())), None), (t(), Empty))
        case _ => None
      })
    }
  }

  sealed trait Stream[+A] {
    import Stream._
    // Exercise 5_1
    def toList: List[A] = {
      this match {
        case Cons(h, t) => h() :: t().toList
        case Empty => Nil
      }
    }

    def drop(n: Int): Stream[A] = {
      this match {
        case Cons(h, t) if (n == 0) => this
        case Cons(h, t) => t().drop(n - 1)
        case Empty => empty
      }
    }

    def forAll(p: A => Boolean): Boolean = {
      //this match {
      //case Cons(h, t)  => p(h()) && t().forAll(p)
      //case Empty => true
      //}
      foldRight(true)((a, b) => p(a) && b)
    }

    def foldRight[B](z: => B)(f: (A, => B) => B): B = {
      this match {
        case Cons(h, t) => f(h(), t().foldRight(z)(f))
        case _ => z
      }
    }

    def take(n: Int): Stream[A] = {
      this match {
        case Empty => Empty
        case Cons(h, t) if (n == 0) => Empty
        case Cons(h, t) => Cons(() => h(), () => t().take(n - 1))
      }
    }

    def takeWhile5_5(p: A => Boolean): Stream[A] = {
      foldRight(Empty: Stream[A])((a, b) => if (p(a)) Cons(() => a, () => b) else b)
    }

    def append[B >: A](l: => Stream[B]): Stream[B] = {
      foldRight(l)((a, b) => Cons(() => a, () => b))
    }


    def flatMap[B](f: A => Stream[B]): Stream[B] = {
      foldRight(Empty: Stream[B])((a, b) => f(a).append(b)
    }

    def map[B](f: A => B): Stream[B] = {
      foldRight(Empty: Stream[B])((a, b) => Cons(() => f(a), () => b))
    }

    def filter(f: A => Boolean): Stream[A] = {
      foldRight(Empty: Stream[A])((a, b) => if (f(a)) Cons(() => a, () => b) else b)
    }

    def headOption5_6[B >: A](f: B => Option[B]): Option[B] = {
      foldRight(None: Option[B])((a, b) => f(a))
    }

    def tails: Stream[Stream[A]] = {
      Stream.unfold((this, None: Option[A]))(x => x match {
        case (Cons(h, t), Some(x)) => Some(Cons(h, t), (t(), Some(h())))
        case (Cons(h, t), None) => Some(Cons(h, t), (t(), Some(h())))
        case (Empty, Some(x)) => Some(Stream(), (empty, None))
        case (Empty, None) => None
      })
    }

  }



    object mn {


      def main(args: Array[String]): Unit = {
        Stream.take(Stream(1, 2, 3, 4, 5, 6), 2).toList
      }
      def test[A](as: Stream[A], s: Stream[A]): Stream[(A, A)] = {
        Stream.unfold((as, s))(x => x match {
          case (Cons(h, t), Cons(h1, t1)) => Some((h(), h1()), (t(), t1()))
          case _ => None
        })
      }


    }



}

