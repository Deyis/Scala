//trait Expr {
//  def isNumber: Boolean
//  def isSum: Boolean
//  def numValue: Int
//  def leftOp: Expr
//  def rightOp: Expr
//}
//
//class Number(n:Int) extends Expr {
//  override def isNumber: Boolean = true
//
//  override def rightOp: Expr = throw new Error("Number.rightOp")
//
//  override def numValue: Int = n
//
//  override def isSum: Boolean = false
//
//  override def leftOp: Expr = throw new Error("Number.leftOp")
//}
//
//class Sum(e1:Expr, e2:Expr) extends Expr {
//
//  override def isNumber: Boolean = false
//
//  override def rightOp: Expr = e2
//
//  override def numValue: Int = throw new Error("Sum.numValue")
//
//  override def isSum: Boolean = true
//
//  override def leftOp: Expr = e1
//}
//
//def eval(e:Expr): Int =
//  if (e.isNumber) e.numValue
//  else if (e.isSum) eval(e.leftOp) + eval(e.rightOp)
//  else throw new Error("unknown expression")
//
//eval(new Sum(new Number(1), new Sum(new Number(2), new Number(2))))

trait Expr {
  def eval: Int
}

class Number(n:Int) extends Expr {
  override def eval: Int = n
}

class Sum(e1:Expr, e2:Expr) extends Expr {
  override def eval: Int = e1.eval + e2.eval
}

new Sum(new Number(1), new Sum(new Number(2), new Number(2))).eval