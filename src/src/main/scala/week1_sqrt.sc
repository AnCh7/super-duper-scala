def abs(x: Double) = if (x < 0) -x else x

def square(x: Double) = x * x

def average(x: Double, y: Double) = (x + y) / 2

def goodEnough(guess: Double, prevGuess: Double) =
  abs(guess - prevGuess) < 0.001

def improve(guess: Double, x: Double) =
  average(guess, x / guess)

def sqrtIter(guess: Double, prevGuess: Double, x: Double): Double =
  if (goodEnough(guess, prevGuess)) guess
  else sqrtIter(improve(guess, x), guess, x)

def sqrt(x: Double) = sqrtIter(1.0, 0, x)

sqrt(0.001)
sqrt(0.1e-20)
sqrt(1.0e20)
sqrt(1.0e50)