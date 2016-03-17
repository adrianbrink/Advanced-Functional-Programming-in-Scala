// Advanced Programming 2015
// Andrzej Wasowski, IT Univesity of Copenhagen
//
// 1. Introduction
//
// This is a superfast walk through the API designed in Chapter 6.
// This exercise assumes that you have read Chapter 6, and it attempts to get
// you up to speed with its main interface, so that we can work on chapter 8.

// Work through the following examples line by line, running them in the REPL,
// understanding and exercising the produced values.  Refer to Chapter 6 and to
// the State.scala file whenever you do not understand.

// 2. Build

// First compile State.scala// Advanced Programming 2015
// $ fsc State.scala
//
// This file is designed so that you can compile it as a regular scala module
// (fsc Exercise1.scala).  This is useful for typechecking the entire file.
// However the intention is that you open the REPL and work through it
// expression by expression. So:
//
// $ scala -i
//
// and paste line by line into the repo (just skip the object declaration line).

package adpro

import fpinscala.state._


object Exercise1 extends App { // this line not needed in REPL

  // RNG is the type of (R)andom (N)umber (G)enerators.  Its companion object
  // (also called RNG) has a factory called "Simple" that allows creating a
  // generator of pseudo random Int numbers.

  val rng1 :RNG = RNG.Simple(42) // 42 is the seed

  // Get a pseudo random number of the generator first (this is the easiest way)
  val (x,rng2) = rng1.nextInt
  // Note that the expression is referentially transparent; try to call it
  // several times. Incidentally this also explains why random number generators
  // are rather called "pseudo random number generators". Why?


  // QUESTION 1: how do I get the next random number? (check in the REPL and make
  // sure that you indeed have got a different number)
  //
  // val ... = ...

  // The book uses this simple generator to implement a series of various
  // generators, including one for nonnegative integers (function
  // RNG.nonNegativeInt) and for doubles (function RNG.double). We will use them
  // below. Check briefly where they are in State.scala, and what are their
  // types. It is slightly less important how they work.

  // The book wraps a code generator in a state pattern.  The state pattern
  // needs a transition function of type RNG => (A, RNG).  So the random number
  // generator state will be the state of our automaton, and the automaton will
  // generate outputs of type A. The function defines how to move from one state
  // to another state producing an output.
  //
  // Now we can package our code generators as automata:
  val s_random_int :State[RNG,Int] = State(_.nextInt)
  val s_nonNegativeInt :State[RNG,Int]= State(RNG.nonNegativeInt)
  val s_double :State[RNG,Double] = State(RNG.double)

  // State gives as a uniform interface to all these generators; The interface
  // is generic, so regardless of what is being randomly generated we access it
  // in the same way.  Give a RNG to a state object and it will produce the next
  // RNG (or more intutiively it will produce the next state of this RNG). Try
  // using them as below:
  s_random_int.run (rng2)
  s_nonNegativeInt.run (rng2)
  s_double.run (rng2)

  // QUESTION 2: once you are comfortable with the above, take one of the other
  // generator functions from State.scala, wrap them into a state, and use the
  // above interface to get a random value out of them.
  //
  // val s_random_x : ... = State (...)
  // val random_x : ... = ....

  // This wrapping makes as independent of the names of the functions of the
  // actual generators, so we can write generic functions for all generators,
  // or we can use even more generic functions implemented just in terms of
  // states.

  // Note that there seems to be a relation between State and Stream.  A
  // continuous execution of State generates a series (a trace) that can be
  // represented using a Stream.  We will use this observation below, to build
  // Streams of random numbers.

  // The following is an example linking States and Streams from the previous
  // exercise session. We will use the above state interface to wrap all random
  // generators into streams.

  // Our stream will be "delayed". It will await a seed (or rather an
  // initialized RNG) to start generation.  Note: I am using standard library
  // streams (which behave as expected)
  def state2stream[A] (s :State[RNG,A]) (seed :RNG) :Stream[A] =
    s.run(seed) match { case (n,s1) => n #:: state2stream (s) (s1) }

  // the function basically takes a transition function and converts it to a
  // stream (it could be written polumorphically  to work for any state.

  // QUESTION 3: generalize the above function to work for any State object, not
  // just for state pattern applied to the random number generators.

  // This hack allows us to hide the state of the random number generator in the
  // stream. We just work with a stream of random numbers declaratively from now
  // on, forgetting that there is some state of the random number generator
  // passed around.

  val random_integers = state2stream[Int] (s_random_int) (RNG.Simple(42))
  // now this is an easy way to generate 10 random numbers
  random_integers.take(10).toList

  // The same code that works with random integer stream can work with no change
  // with random stream of non-negative integers (the stream is computed using
  // the same state2stream function)
  val random_non_negative_integers =
    state2stream[Int] (s_nonNegativeInt) (RNG.Simple(42))
  random_non_negative_integers.take(10).toList

  // And we can use the same function to create a stream of random Doubles
  val random_doubles = state2stream[Double] (s_double) (RNG.Simple(42))
  random_doubles.take(10).toList

  // If you need a reasonably reliable seed, so that you do not get exactly
  // stream each time, use currentTimeMillis:
  val random_integers_no_RT =
    state2stream[Int] (s_random_int) (RNG.Simple(System.currentTimeMillis.toInt))

  // The above expression is of course not referentially transparent. If we
  // substitute the computation for the identifier in each reference place, we
  // will get a different value each time! The one below already is RT; it will
  // retain the same stream value throughout the program)
  random_integers_no_RT.take(10).toList

  // QUESTION 4: Now repeat the above by taking the random generator function
  // that you used in Question 2, combine it with the above API to create a
  // stream of random values that it generates. Access the stream to compute
  // several random values.

}

// vim:cc=80:foldmethod=indent:foldenable:tw=80
