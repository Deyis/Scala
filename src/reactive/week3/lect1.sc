
import scala.concurrent.Future
import scala.util.{Try, Success, Failure}

//synchonized

trait Adventure {
  type Coin = Int
  type Treasure
//  def collectCoins(): List[Coin]
//  def buyTreasure(coins: List[Coin]):Treasure

  def collectCoins(): Try[List[Coin]]
  def buyTreasure(coins: List[Coin]):Try[Treasure]
}


trait Socket {
  private def delay(s: Int):Unit = {
    println(s"delay: $s")
  }
  def readFromMemory(): Future[Array[Int]] = Future {
      delay(3)
      Array(1,2,3,4)
    }

  def sendToEurope(packet: Array[Int]): Future[Array[Int]] = Future {
    delay(5*365)
    Array(1,2,3,4)
  }
}

val s = new Socket {}
val packet = s.readFromMemory()
//packet onComplete {
//  case Success(p) => val confirmation = s.sendToEurope(p)
//}
//val confirmation = packet.flatMap(p => s.sendToEurope(p))

val confirmation = for {
  p <- s.readFromMemory()
  c <- s.sendToEurope(p)
} yield c

//def retry[T](noTimes: Int)(block: => Future[T]): Future[T] = {
//  if(noTimes == 0 ) {
//    Future.failed(new Exception("Sorry"))
//  } else {
//    block fallbackTo {
//      retry(noTimes -1) {block}
//    }
//  }
//}

//def retry[T](noTimes: Int)(block: => Future[T]): Future[T] = {
//  val ns : Iterator[Int] = (1 to noTimes).iterator
//  val attempts: Iterator[Future[T]] = ns.map(_=>  block )
//  val failed = Future.failed(new Exception("Sorry"))
//
//  attempts.foldLeft(failed)( (a,block) => a recoverWith { block })
//}


//def retry[T](noTimes: Int)(block: => Future[T]): Future[T] = async {
//  var i = 0
//  var result: Try[T] = Failure(new Exception(""))
//  while (i < noTimes && result.isFailure){
//    result = await {Try(block)}
//    i += 1
//  }
//  result.get
//}

