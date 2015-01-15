val vocabulary = List(
  "Java", "Kata", "Lava", "Book",
  "Scala", "is", "fun", "Rackc")

val mnemonic = Map (
  '2' -> "ABC", '3' -> "DEF" , '4' -> "GHI" ,
  '5' -> "JKL" , '6' -> "MNO" , '7' -> "PQRS" ,
  '8' -> "TUV" , '9' -> "WXYZ"
)
val charCode: Map[Char, Char] =
  for (
    (digit, str) <- mnemonic;
    letter <- str)
  yield letter -> digit

def wordCode(word:String): String =
  word.toUpperCase map charCode

wordCode("java")
def wordsForNum: Map[String, Seq[String]] =
 vocabulary groupBy wordCode withDefaultValue Seq()

def encode(number: String): Set[List[String]] =
 if (number.isEmpty) Set(List())
 else {
   (for {
     split <- 1 to number.length
     word <- wordsForNum(number take split)
     rest <- encode(number drop split)
   } yield word::rest).toSet
 }


def translate(number:String): Set[String] =
 encode(number) map ( _ mkString " ")

val check = "7225247386"
encode(check)
translate(check)
