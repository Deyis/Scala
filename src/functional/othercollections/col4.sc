case class Book(title: String, authors: List[String])

val books = Set(
  Book(title = "A", authors = List("A")),
  Book("C", List("CD")),
  Book("B", List("B")),
  Book("D", List("CD")),
  Book("E", List("CD"))
)

for (b <- books) yield b.title

for (b <- books; a<- b.authors if a startsWith "B") yield b.title

for {
  b1 <- books
  b2 <- books
  if b1.title < b2.title
  a1 <- b1.authors
  a2 <- b2.authors
  if a1 == a2
} yield a1

//Translate to High order function
for (b <- books; a<- b.authors if a startsWith "B") yield b.title
books.flatMap(b => b.authors.withFilter( a => a startsWith "B" ).map(y => b.title))

