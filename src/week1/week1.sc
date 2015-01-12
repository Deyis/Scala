def sum(a: Double, b: Double): Double = a + b

def square(x: Double): Double = x * x
def cub(x: Double): Double = x * x * x

def sumOf(f: Double => Double, f2: Double => Double)
         (a: Double, b: Double): Double = sum(f(a), f2(b))

sum(square(2), square(2))
sumOf(square, cub)(2, 2)
sumOf(cub, square)(2, 2)

// -----------------------------------------------------------


class Rational(x:Int, y:Int) {
  require(y != 0, "denominator must be non zero")

  def this(x:Int) = this(x, 1)

  private def gcd(a:Int, b:Int) : Int = if (b == 0) a else gcd(b, a%b)
  private val g = gcd(x,y)
  def numer = x / g
  def denom = y / g


  def +(that: Rational) =
    new Rational(numer * that.denom + that.numer * denom, denom * that.denom)

  def unary_- :Rational = new Rational(-numer, denom)

  def -(that:Rational) = this + -that

  def <(that: Rational) = numer * that.denom < that.numer * denom

  def max(that:Rational) = if(this < that) that else this

  override def toString() = {
    numer + "/" + denom
  }
}

val x = new Rational(1,3)
val y = new Rational(5,7)
val z = new Rational(3,2)
x - y - z

val d = new Rational(1,2)
d + d


