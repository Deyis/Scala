// merge sort

//def msort(xs:List[Int]): List[Int] = {
//  val n = xs.length /2
//  if (n == 0) xs
//  else {
//
//    def merge(xs: List[Int], ys: List[Int]): List[Int] = (xs, ys) match {
//      case (Nil, ys1) => ys
//      case (xs1, Nil) => xs
//      case (x :: xs1, y :: ys1) =>
//          if(x < y) x :: merge(xs1, ys)
//          else y :: merge(xs, ys1)
//    }
//
//    val (frst, scnd) = xs splitAt n
//    merge(msort(frst), msort(scnd))
//  }
//}
//
//msort(List(2, -4, 5, 7, 1))

//BECAUSE OF IMPLICIT PARAMETER
def msort[T](xs:List[T])(implicit ord: Ordering[T]): List[T] = {
  val n = xs.length /2
  if (n == 0) xs
  else {

    def merge(xs: List[T], ys: List[T]): List[T] = (xs, ys) match {
      case (Nil, ys1) => ys
      case (xs1, Nil) => xs
      case (x :: xs1, y :: ys1) =>
        if(ord.lt(x, y)) x :: merge(xs1, ys)
        else y :: merge(xs, ys1)
    }

    val (frst, scnd) = xs splitAt n
    merge(msort(frst), msort(scnd))
  }
}

msort(List(2, -4, 5, 7, 1))
msort(List("apple", "pine", "orange", "banana"))

