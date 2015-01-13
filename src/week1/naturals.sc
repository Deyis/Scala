abstract class Nat {
  def isZero: Boolean
  def predecessor: Nat
  def successor: Nat = new Succ(this)
  def +(other: Nat): Nat
  def -(other: Nat): Nat
}
object Zero extends Nat {
  override def isZero: Boolean = true
  override def predecessor: Nat = throw new Error("0.predecessor")
  override def +(other: Nat): Nat = other
  override def -(other: Nat): Nat =
    if(other.isZero) this
    else throw new Error("0.predecessor")
}
class Succ(n: Nat) extends Nat {
  override def isZero: Boolean = false
  override def +(other: Nat): Nat = new Succ(n + other)
  override def -(other: Nat): Nat =
    if(other.isZero) this
    else n - other.predecessor
  override def predecessor: Nat = n
}
val s = Zero.successor + Zero.successor
val t = s - Zero.successor
t.predecessor

val t2 = t - Zero.successor
t2.predecessor




