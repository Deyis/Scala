
// monads

trait  M[T] {
  def flatMap[U](f: T => M[U]) : M[U]

  def unit[T](x: T): M[T]

  def map[U](f: T => U) : M[U] = {
    flatMap( x => unit(f(x)))
  }
}


abstract class Try[+T] {

  def flatMap[U](f: T => Try[U]): Try[U] = this match {
    case Succes(x) => try f(x) catch { case NoneFatal(ex) => new Fail(ex) }
    case Fail(x) => new Fail(x)
//    case fail: Fail => fail
  }

  def map[U](f: T => U): Try[U] = this match {
//    case Succes(x) => Try(f(x))
//    case fail: Fail => fail
    case Succes(x) => try new Succes[U](f(x)) catch { case NoneFatal(ex) => new Fail(ex) }
    case Fail(x) => new Fail(x)
  }

}
case class Succes[T](x:T) extends Try[T]
case class Fail[T](ex:Exception) extends Try[Nothing]

case class NoneFatal(ex: Exception) extends Exception


object Try extends Try {

  def apply[T](expr: => T): Try[T] =
    try new Succes(expr)
    catch {
      case NoneFatal(ex) => new Fail(ex)
    }
}

for {
  i <- Try(0 to 10)
} yield i

for {
  i <- Try({throw new NoneFatal(new Exception) })
} yield i

