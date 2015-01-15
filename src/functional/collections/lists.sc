//val l = List()
//val l2 = List()
//l.length
//l.last
//l.init

//l take 2 //before index
//l drop 1 // after index
//l(0) // index

//l ++ l2 //concatination
//l ::: l2 //concatination ???

//l.reverse
//l.updated(0, 1)
//
//l indexOf 2
//l contains 3

def last[T](xs: List[T]): T = xs match {
  case List() => throw new Error("last of empty list")
  case List(x) => x
  case y :: ys  => last(ys)
}

def concat[T](xs:List[T], ys: List[T]): List[T] = xs match {
  case List() => ys
  case z :: zs => z :: concat(zs, ys)
}

def reverse[T](xs:List[T]): List[T] = xs match {
  case List() => List()
  case z :: zs => reverse(zs) ++ List(z)
}

val list1 = List(1,2,3,4)
val list2 = List(1,2,3,4)
concat(list1, reverse(list2))

def removeAt[T](n:Int, xs:List[T]): List[T] = (xs take n) ::: (xs drop n + 1)
removeAt(1, List(0,1,2,3))
def removeAt2[T](n:Int, xs:List[T]): List[T] = (xs take n) ++ (xs drop n + 1)
removeAt2(1, List(0,1,2,3))