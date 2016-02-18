// This is not legal Scala code, but an example to be read and
// experimented with. It does not parse, since it shows incomplete
// code fragments

def factorial (n :Int) :Int = {
  var result = 1
  var x = n
  while (x > 1) {
    result *= x
    x -= 1
  }
  return result
}

// we extract the state we operate on: x, result and turn it into
// parameters, which are additional to the one we already head

// the return type stays the same

def doWork (n :Int) (result :Int) (x: Int) :Int

// now we turn the loop into an if-condition that will decide to
// loop or recurse

def doWork (n :Int) (result :Int) (x: Int) :Int
  if (x > 1) /* translate loopbody here */
  else /* translate the rest here */

// now we translate the loop body
def doWork (n :Int) (result :Int) (x: Int) :Int
  if (x > 1) doWork (n) (result*x) (x-1)
  else result

// now we need to hide the technical parameters
// and implement the initialization from the original function
def factorial (n :Int) :Int = {
  def doWork (n :Int) (result :Int) (x: Int) :Int
    if (x > 1) doWork (n) (result*x) (x-1)
    else result
  doWork (n) (1) (n)
}

//  n is invariant so we can eliminate it from doWork's explicit
//  interface
def factorial (n :Int) :Int = {
  def doWork (result :Int) (x: Int) :Int
    if (x > 1) doWork (result*x) (x-1)
    else result
  doWork (1) (n)
}


