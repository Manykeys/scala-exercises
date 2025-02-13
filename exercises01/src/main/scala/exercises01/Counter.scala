package exercises01

import scala.util.matching.Regex

object Counter {

  /**
    * Посчитать количество вхождений слов в тексте
    * слово отделено символами [\s.,!?:\n\t\r]
    */
  def countWords(text: String): Map[String, Int] = {
    val regex: Regex = raw"[^\s.,!?:\n\t\r()]+".r
    regex
      .findAllIn(text.toLowerCase)
      .toList
      .groupMapReduce(identity)(_ => 1)(_ + _)
  }

  /**
    * Посчитать количество вхождений английских слов в тексте
    * слово отделено символами [\s.,!?:\n\t\r]
    */
  def countEnglishWords(text: String): Map[String, Int] = {
    val regex: Regex = raw"[^\s.,!?:\n\t\r()]+".r
    regex
      .findAllIn(text.toLowerCase)
      .filter(x => x.forall(y => if (y.isLetter) isLatinLetter(y) else true))
      .toList
      .groupMapReduce(identity)(_ => 1)(_ + _)
  }

  private def isLatinLetter(c: Char): Boolean =
    (c >= 'a' && c <= 'z') || (c >= 'A' && c <= 'Z')

  /**
    * Посчитать количество вхождений чисел в тексте
    * число отделено символами [\s!?:\n\t\r]
    */
  def countNumbers(text: String): Map[String, Int] = {
    val regex: Regex = raw"[^\s!?:\n\t\r]+".r
    regex
      .findAllIn(text.toLowerCase)
      .filter(x => x.map(y => !y.isLetter).foldLeft(true)(_ & _))
      .toList
      .groupMapReduce(identity)(_ => 1)(_ + _)
  }
}
