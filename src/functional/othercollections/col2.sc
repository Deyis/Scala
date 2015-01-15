// combinatorial search
def isPrime(n: Int): Boolean =
  (2 until n).forall( (i)=> n % i != 0 )

val n = 7

(1 until n) flatMap ( i =>
  (1 until i) map ( j => (i,j))) filter (pair => isPrime(pair._1 + pair._2))

(1 until n) flatMap ( i =>
  (1 until i) map ( j => (i,j))) filter {case (x,y) => isPrime(x + y)}

for {
  i <- 1 until n
  j <- 1 until i
  if isPrime(i + j)
} yield (i, j)

def scalarProduct (xs: Vector[Double], ys: Vector[Double]) : Double =
(xs zip ys).map{case (x,y) => x * y}.sum

// это вложенный цикл!!!
def scalarProduct2 (xs: Vector[Double], ys: Vector[Double]) : Double =
  (for { x <- xs; y <- ys} yield x * y).sum

def scalarProduct3 (xs: Vector[Double], ys: Vector[Double]) : Double =
  (for { (x, y) <- xs zip ys} yield x * y).sum

scalarProduct(Vector(1,2), Vector(1,2))
// Неправильно!
scalarProduct2(Vector(1,2), Vector(1,2))
scalarProduct3(Vector(1,2), Vector(1,2))