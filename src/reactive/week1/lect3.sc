import java.util.Random
trait Generator[T] {
  self =>

  var ffilter: T => Boolean = defaultFilter
  def defaultFilter(t: T): Boolean = {
    true
  }

  def generate: T

  def map[S](f: T => S): Generator[S] = new Generator[S] {
      override def generate: S = {
        val a = self.generate
        if (self.ffilter(a)) {
          self.ffilter = self.defaultFilter
          f(a)
        } else
          generate
      }
    }

  def flatMap[S](f: T => Generator[S]):Generator[S] = new Generator[S] {
    override def generate: S = f(self.generate).generate
  }

  def withFilter[S](f : T => Boolean): Generator[T] = {
    ffilter = f
    this
  }
}

val integers = new Generator[Int] {
  val rand = new Random()
  override def generate: Int = rand.nextInt
}

val bools = new Generator[Boolean] {
  override def generate = integers.generate > 0
}

val bools2 = for(x <- integers) yield x > 0

val pairs2 = for (x <- integers; y <- integers) yield (x, y)

def choose(lo:Int, hi:Int): Generator[Int] =
  for(x <- integers if x >= 0) yield lo + x % (hi - lo)

def choose2(lo:Int, hi:Int): Generator[Int] =
  integers.withFilter(y => y >= lo).map(x => lo + x % (hi - lo))

def oneOf[T](xs: T*): Generator[T] =
  for(idx <- choose(0, xs.length)) yield xs(idx)

def single[T](x:T) : Generator[T] = new Generator[T] {
  override def generate: T = x
}
def emptyLists = single(Nil)
def nonEmptyLists = for(head <- integers; tail <- lists) yield head :: tail
def lists: Generator[List[Int]] = for {
  isEmpty <- bools2
  lists <- if(isEmpty) emptyLists else nonEmptyLists
} yield lists

for ( i <- 0 until 10 ) yield lists.generate
for ( i <- 0 until 10 ) yield bools.generate

for ( i <- 0 until 10 ) yield oneOf("apple", "orange", "pineapple").generate
for ( i <- 0 until 10 ) yield bools.generate



