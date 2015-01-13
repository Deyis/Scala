import java.util.NoSuchElementException

// enable covariance
trait List[+T] {
  def isEmpty: Boolean
  def head: T
  def tail: List[T]
//  method parameters is contravariant, returning type - covariant
  def prepend[E >: T](elem: E): List[E] = new Cons[E](elem, this)
}

class Cons[T](val head: T, val tail: List[T]) extends List[T] {
  override def isEmpty: Boolean = false
}

object Nil extends List[Nothing] {
  override def isEmpty: Boolean = true
  override def tail: Nothing = throw new NoSuchElementException("Nil.tail")
  override def head: Nothing = throw new NoSuchElementException("Nil.head")
}

val x: List[String] = Nil
