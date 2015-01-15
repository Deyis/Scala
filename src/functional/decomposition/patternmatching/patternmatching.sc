trait  Expr {
  //first that match
  def eval: Int = this match  {
    case Number(n) => n
    //this will be match for eval(Sum(Number(0), Number(1)))
    case Sum(Number(0), e:Expr) => e.eval
    case Sum(Number(0), Number(n)) => n
    case Sum(e1:Expr, e2:Expr) => e1.eval + e2.eval
  }
}

case class Number(n:Int) extends Expr
case class Sum(e1:Expr, e2:Expr) extends Expr

Sum(Number(0), Number(1)).eval


def show(e: Expr): String = e match {
  case Number(n) => n.toString
  case Sum(e1:Expr, e2:Expr) => "(" + show(e1) + " + " + show(e2) + ")"
}

show(Sum(Number(1), Sum(Number(2), Number(3))))





