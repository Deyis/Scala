val s = "Hello World"
s exists (c=> c.isUpper)
s forall (c=> c.isUpper)

val pairs = List(1,2,3) zip s
pairs.unzip

s flatMap( c=> List('.', c))

def scalarProduct (xs: Vector[Double], ys: Vector[Double]) : Double =
  (xs zip ys).map{case (x,y) => x * y}.sum

def isPrime(n: Int): Boolean =
  (2 until n).forall( (i)=> n % i != 0 )

isPrime(3)
isPrime(4)
