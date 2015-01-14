// reduction of list

def sum(xs: List[Int]): Int = xs match {
  case Nil => 0
  case y::ys => y + sum(ys)
}

def sum2(xs: List[Int]) : Int = (0 :: xs) reduceLeft(_ + _)

def product(xs: List[Int]) : Int = (1 :: xs) reduceLeft(_ * _)
def product2(xs: List[Int]) : Int = (1 :: xs) reduceLeft((x,y) => x*y)
def product3(xs: List[Int]) : Int = (xs foldLeft 1)((x,y) => x*y)


def concat[T](xs: List[T], ys:List[T]): List[T] =
  (xs foldRight ys)((x, list) => x :: list)
//  (xs foldRight ys)(_ :: _ )


def concatReversed[T](xs: List[T], ys:List[T]): List[T] =
  (xs foldLeft ys)((list, x) =>  x :: list)
//  (xs foldLeft ys)(_.::(_))

List(1,2,3)
concat(List(1,2,3),List(1,2,3))
concatReversed(List(1,2,3),List(1,2,3))


def mapFun[T, U](xs:List[T], f: T => U): List[U] =
//  (xs foldRight List[U]())((x, acc)=> f(x) :: acc)
  (xs foldRight List[U]())(f(_)::_)
mapFun[Int, Int](List[Int](1,2,3), (x) => x * x)

def lengthFun[T](xs: List[T]): Int =
  (xs foldRight 0)((_, acc) => acc + 1)

lengthFun(List(1,2,3))