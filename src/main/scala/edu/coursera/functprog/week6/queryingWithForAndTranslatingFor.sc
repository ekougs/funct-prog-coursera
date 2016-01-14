import edu.coursera.functprog.week6.Book

val books = List(
  Book(title = "Structure and Interpretation of Computer Programs",
    authors = List("Abelson, Harald", "Sussman, Gerald J.")),
  Book(title = "Introduction to Functional Programming",
    authors = List("Bird, Richard", "Walder, Phil")),
  Book(title = "Effective Java",
    authors = List("Bloch, Joshua")),
  Book(title = "Java Puzzlers",
    authors = List("Bloch, Joshua", "Gafter, Neal")),
  Book(title = "Programming in Scala",
    authors = List("Odersky, Martin", "Spoon, Lex", "Venners, Bill"))
)

// Books whose authors is Bird
for (book <- books; author <- book.authors if author startsWith "Bird,") yield book.title
// Higher order translation of this query
books.flatMap(book => book.authors.withFilter(author => author startsWith "Bird,").map(author => book.title))
// Books which have the word program in it
for (book <- books if book.title contains "Program") yield book.title
// Authors who have written at least 2 books
(
  for {
    b1 <- books
    b2 <- books
    if b1 != b2
    a1 <- b1.authors
    a2 <- b2.authors
    if a1 == a2
  } yield a1
  ).distinct


val fruits = List("apple", "pear", "orange", "pineapple")
// Tri personnalisé
fruits sortWith (_.length < _.length)
// Tri naturel
fruits.sorted
// Same as fruits groupBy (fruit => fruit.head)
fruits groupBy (_.head)