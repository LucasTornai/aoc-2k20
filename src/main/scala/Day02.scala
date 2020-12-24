import Day01.input

import scala.io.Source

object Day02 extends App {

  val input = Source.fromResource("Input02")
  val inputLines = input.getLines().toArray
  input.close()

  val parsedEntriesPart01 = inputLines.map { x=>
    val firstSplit = x.split(": ")
    val password = firstSplit(1)

    val secondSplit = firstSplit(0).split(" ")
    val character = secondSplit(1).head

    val thirdSplit = secondSplit(0).split("-")
    val min = thirdSplit(0).toInt
    val max = thirdSplit(1).toInt

    val appearances = password.count(_ == character)
    if (appearances >= min && appearances <= max) {
      true
    } else {
      false
    }
  }

  println(parsedEntriesPart01.count(_ == true))

  val parsedEntriesPart02 = inputLines.map { x=>
    val firstSplit = x.split(": ")
    val password = firstSplit(1)

    val secondSplit = firstSplit(0).split(" ")
    val character = secondSplit(1).head

    val thirdSplit = secondSplit(0).split("-")
    val first = thirdSplit(0).toInt
    val second = thirdSplit(1).toInt

    val firstTry = password.lift(first - 1)
    val secondTry = password.lift(second - 1)
    if (firstTry.nonEmpty && firstTry.get == character) {
      if (secondTry.isEmpty || secondTry.get != character) {
        true
      }
    } else if (secondTry.nonEmpty && secondTry.get == character) {
      true
    } else {
      false
    }
  }

  println(parsedEntriesPart02.count(_ == true))
}
