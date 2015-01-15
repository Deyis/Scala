def squareList(xs: List[Int]): List[Int] = xs match {
  case Nil => Nil
  case y :: ys => y*y :: squareList(ys)
}

def squareList2(xs: List[Int]): List[Int] =
  xs map ((x) => x*x )

squareList(List(1,2,3,4))
squareList2(List(1,2,3,4))


def posElem(xs:List[Int]) : List[Int] = xs match {
  case Nil => Nil
  case y::ys => if (y > 0) y::posElem(ys) else posElem(ys)
}

posElem(List(-1,1,-2,2,-3,3))

List(-1,1,-2,2,-3,3) filter (x=> x > 0)
List(-1,1,-2,2,-3,3) filterNot (x=> x > 0)
List(-1,1,-2,2,-3,3) partition (x=> x > 0)

List(-1,1,-2,2,-3,3) takeWhile (x=> x<0)
List(-1,1,-2,2,-3,3) dropWhile (x=> x<0)

List(-1,1,-2,2,-3,3) span (x=> x<0)


def pack[T](xs: List[T]): List[List[T]] = xs match {
  case Nil => Nil
  case x :: xs1 => {
    val (frst, scnd): (List[T], List[T]) = xs.span(y => y == x)
    frst :: pack(scnd)
  }
}

pack(List("a", "a", "a", "b", "c", "c", "a"))

def encode[T](xs: List[T]): List[(T, Int)] =
  pack(xs) map( ys => (ys.head, ys.length))

encode(List("a", "a", "a", "b", "c", "c", "a"))

