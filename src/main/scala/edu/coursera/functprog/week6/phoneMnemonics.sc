import scala.io.Source

val mnemonics = Map('2' -> "ABC", '3' -> "DEF", '4' -> "GHI", '5' -> "JKL", '6' -> "MNO", '7' -> "PQRS", '8' -> "TUV",
  '9' -> "WXYZ")
val charDigit: Map[Char, Char] = {
  for {
    (digit, letters) <- mnemonics
    letter <- letters
  } yield letter -> digit
}
def allCharsInMnemonics(word: String): Boolean = {
  word forall (char => charDigit contains char)
}
val in = Source.fromURL("file:/usr/share/dict/words")
// Takes words with at least 2 characters otherwise computation is way too slow !!!
val words = in.getLines.toList.map(word => word.toUpperCase).filter(allCharsInMnemonics).filter(_.length > 1)
def wordCode(word: String): String = {
  word.toUpperCase map (char => charDigit(char))
}
wordCode("Java")

val wordsForNum: Map[String, Seq[String]] = {
  words groupBy wordCode withDefaultValue Seq()
}
def encode(number: String): Set[List[String]] = {
  if (number.isEmpty) Set(List())
  else {
    for {
      split <- 1 to number.length
      word <- wordsForNum(number take split)
      rest <- encode(number drop split)
    } yield
      word :: rest
  }.toSet
}
def translate(number: String) = {
  encode(number) map (_.mkString(" "))
}
translate("7225247386")