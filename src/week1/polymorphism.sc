import java.util.NoSuchElementException

trait List[T] {
  def isEmpty: Boolean
  def head: T
  def tail: List[T]
}

class Cons[T](val head: T, val tail: List[T]) extends List[T] {
  override def isEmpty: Boolean = false
}

class Nil[T] extends List[T] {
  override def isEmpty: Boolean = true
  override def tail: Nothing = throw new NoSuchElementException("Nil.tail")
  override def head: Nothing = throw new NoSuchElementException("Nil.head")
}

def singleton[T](elem: T) = new Cons[T](elem, new Nil[T])
singleton(1)

def nth[T](n:Int, list:List[T]): T = {
  if(list.isEmpty ||  n < 0) {
    throw new IndexOutOfBoundsException
  } else if (n == 0) {
    list.head
  } else {
    nth(n-1, list.tail)
  }
}

val list:List[Int] = new Cons[Int](0,new Cons[Int](1, new Cons[Int](2, new Nil[Int])))
nth(0,list)
nth(1,list)
nth(2,list)


