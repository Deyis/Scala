def expr = {
  val x = { print("x"); 1 }
  lazy val y = { print("y"); 2 }
  def z = { print("z"); 3 }
  z + y + x + z + y + x
}
expr

def from(n:Int) : Stream[Int] = Stream.cons(n, from(n+1))
val nats = from(0)
val m4 = nats map( _ * 4)

(m4 take 10).toList

def sieve(s: Stream[Int]): Stream[Int] =
  s.head #:: sieve(s.tail filter (_ % s.head != 0))


val primes = sieve(from(2))

(primes take 100).toList
