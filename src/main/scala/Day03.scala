import scala.io.Source

object Day03 extends App{
  val input = Source.fromResource("Input03")
  val inputLines = input.getLines().toArray
  input.close()

  val lineLenght = inputLines.head.length

  def runWithSlope(right: Int, down: Int): Long = {
    val whereList = LazyList.from(0).map(x => (x * right) % lineLenght)

    val hits = inputLines.zipWithIndex.filter { case (_, index) =>
      index % down == 0
    }.map { case (x, _) => x }.zipWithIndex.map { case (x, index) =>
      if (x(whereList(index)) == '#') {
        true
      } else {
        false
      }
    }

    hits.count(_ == true)
  }

  println(runWithSlope(3, 1))

  println(runWithSlope(1, 1) * runWithSlope(3, 1) * runWithSlope(5, 1) * runWithSlope(7, 1) * runWithSlope(1, 2))
}
