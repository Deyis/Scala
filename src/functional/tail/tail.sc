def gcd(a:Int, b:Int): Int =
  if (b == 0) a else gcd(b, a % b)

def factorial(n: Int): Int =
 if (n == 0) 1 else n * factorial(n - 1)

def tailFactorial(n: Int, acc: Int): Int =
  if (n == 0) acc else tailFactorial(n - 1, acc * n)


def tailFactorial2(n: Int): Int = {
  def loop(acc:Int, n:Int): Int = {
    if (n == 0) acc
    else loop(acc * n, n - 1)
  }
  loop(1, n)
}

factorial(4)
tailFactorial(4, 1)
tailFactorial2(4)

