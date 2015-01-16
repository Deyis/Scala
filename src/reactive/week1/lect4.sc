// monads

trait  M[T] {
  def flatMap[U](f: T => M[U]) : M[U]
  def unit[T](x: T): M[T]
  def map[U](f: T => U) : M[U] = {
    flatMap( x => unit(f(x)))
  }
}



