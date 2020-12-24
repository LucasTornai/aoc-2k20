import scala.annotation.tailrec
import scala.io.Source

object Day01 extends App {

  val input = Source.fromResource("Input01")
  val inputLines = input.getLines().toArray.map(_.toLong)
  input.close()

  @tailrec
  def findTwo(values: Array[Long]): Long = {
    val head = values.head
    val other = values.tail.dropWhile(x => x + head != 2020)

    if (other.nonEmpty) {
      head * other.head
    } else {
      findTwo(values.tail)
    }
  }

  @tailrec
  def findTwoWithPivot(pivot: Long, values: Array[Long]): Long = {
    if (values.length < 2) {
      return -1
    }

    val secondPivot = values.head
    val other = values.tail.dropWhile(x => pivot + secondPivot + x != 2020)

    if (other.nonEmpty) {
      pivot * secondPivot * other.head
    } else {
      findTwoWithPivot(pivot, values.tail)
    }
  }

  def findThree(values: Array[Long]): Long = {
    values.map(x => findTwoWithPivot(x, values)).dropWhile(_ == -1).head
  }

  println(findTwo(inputLines))
  println(findThree(inputLines))
}