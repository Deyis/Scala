abstract class IntSet {
  def incl(x: Int) : IntSet
  def contains(x: Int) : Boolean
  def union(other: IntSet): IntSet
}

object Empty extends IntSet {

  override def incl(x: Int): IntSet = new NonEmpty(x, Empty, Empty)
  override def contains(x: Int): Boolean = false
  override def union(other: IntSet): IntSet = other
  override def toString = "."
}

class NonEmpty(elem: Int, left: IntSet, right: IntSet) extends IntSet {

  override def incl(x: Int): IntSet =
    if(x < elem) new NonEmpty(elem, left incl x, right)
    else if (x > elem) new NonEmpty(elem, left, right incl x)
    else this

  override def contains(x: Int): Boolean =
    if(x < elem) left contains x
    else if(x > elem) right contains x
    else true

  override def toString = "{" + left + elem + right + "}"

  override def union(other: IntSet): IntSet =
    left.union(right).union(other).incl(elem)
}

val t1 = new NonEmpty(3, Empty, Empty) incl 4
val t2 = new NonEmpty(5, Empty, Empty) incl 2

t1 union t2


