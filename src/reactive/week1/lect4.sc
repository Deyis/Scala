// monads

trait  M[T] {
  def flatMap[U](f: T => M[U]) : M[U]

  def unit[T](x: T): M[T]

  def map[U](f: T => U) : M[U] = {
    flatMap( x => unit(f(x)))
  }
}


object Try extends Try[Any] {

  def apply[T](expr: => T): Try[T] =
    try new Succes[T](expr)
    catch {
      case NoneFatal(ex) => new Fail(ex)
    }
}

abstract class Try[+T] {

  def flatMap[U](f: T => Try[U]): Try[U] = {
    print(" flatMap ")
    this match {
      case Succes(x) => f(x)//try f(x) catch { case NoneFatal(ex) => new Fail(ex) }
      case fail: Fail => fail
    }
  }

  def map[U](f: T => U): Try[U] = {
    print(" map ")
    this match {
      case Succes(x) => Try(f(x))
      case fail: Fail => fail
    }
  }
}
case class Succes[T](x:T) extends Try[T]
case class Fail(ex:Exception) extends Try[Nothing]

case class NoneFatal(ex: Exception) extends Exception


for {
  i <- Try(0 to 10)
} yield i

for {
  i <- Try(0 to 10)
  j <- Try(0 to 10)
} yield i

for {
  i <- Try(0 to 10)
  j <- Try({throw new NoneFatal(new Exception) })
} yield i

for {
  j <- Try({throw new NoneFatal(new Exception) })
  i <- Try(0 to 10)
} yield i

for {
  i <- Try({throw new NoneFatal(new Exception) })
} yield i

