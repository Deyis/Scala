//List are immutable
//List are recursive, while arrays are flat

val fruits = "apples" :: ("oranges" :: ("pears" :: Nil))

// the same
1 :: 2 :: 3 :: Nil
Nil.::(3).::(2).::(1)

fruits.isEmpty
fruits.head
fruits.tail.head

val l = List(7, 3, 9, 2)

def insert(x:Int, xs:List[Int]):List[Int] = xs match  {
  case List() => List(x)
  case y :: ys =>
    if (x <= y) x::xs
    else y :: insert(x, ys)
}

def isort(xs:List[Int]):List[Int] = xs match  {
  case List() => List()
  case y:: ys => insert(y, isort(ys))
}

isort(l)
