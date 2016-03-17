// FINAL
//
// Advanced Programming, Exercises by A. Wąsowski, IT University of Copenhagen
//
// AUTHOR1: Andrea Bitzili - abit@itu.dk
// AUTHOR2: Adrian Brink - adbr@itu.dk
// Group number: 43
//
// A script meant to be loaded into REPL (scala -i Main.scala)

import fpinscala.laziness._
import fpinscala.laziness.Stream._

// this is how we do simple interactive testing
// import exercises.laziness._
val l1 :Stream[Int] = Empty
val l2 :Stream[Int] = empty

val l3 :Stream[Int]= cons(1, cons(2, cons (3, cons(4, empty))))

val naturals: Stream[Int] = Stream.from(1)

println (l1.headOption)
println (l2.headOption)
println (l3.headOption)


