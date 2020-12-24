import scala.io.Source

case class Field(field: String, value: String)
case class Hgt(t: String, value: Int)

object Day04 extends App {
  val input = Source.fromResource("Input04")
  val inputLines = input.getLines().toArray
  input.close()

  val inputDocuments = inputLines.mkString("!!").split("!!!!")

  val hits = inputDocuments.map { x =>
    if (x.contains("byr:") &&
        x.contains("iyr:") &&
        x.contains("eyr:") &&
        x.contains("hgt:") &&
        x.contains("hcl:") &&
        x.contains("ecl:") &&
        x.contains("pid:")) {
      true
    } else {
      false
    }
  }

  println(hits.count(_ == true))

  def parseField(field: String): Field = {
    val split = field.split(":")

    Field(split(0).trim, split(1).trim)
  }

  def checkDocument(fields: Seq[Field]): Boolean = {
    val byrMaybe = fields.find(_.field == "byr")
    val iyrMaybe = fields.find(_.field == "iyr")
    val eyrMaybe = fields.find(_.field == "eyr")
    val hgtMaybe = fields.find(_.field == "hgt")
    val hclMaybe = fields.find(_.field == "hcl")
    val eclMaybe = fields.find(_.field == "ecl")
    val pidMaybe = fields.find(_.field == "pid")

    if (byrMaybe.nonEmpty && iyrMaybe.nonEmpty && eyrMaybe.nonEmpty && hgtMaybe.nonEmpty && hclMaybe.nonEmpty && eclMaybe.nonEmpty && pidMaybe.nonEmpty) {
      val byr = byrMaybe.get.value
      if (byr.length != 4 || byr.toInt < 1920 || byr.toInt > 2002) {
        return false
      }

      val iyr = iyrMaybe.get.value
      if (iyr.length != 4 || iyr.toInt < 2010 || iyr.toInt > 2020) {
        return false
      }

      val eyr = eyrMaybe.get.value
      if (eyr.length != 4 || eyr.toInt < 2020 || eyr.toInt > 2030) {
        return false
      }

      val hgt = hgtMaybe.get.value
      val hgtValue = if (hgt.contains("cm")) {
        Hgt("cm", hgt.split("cm")(0).toInt)
      } else if (hgt.contains("in")) {
        Hgt("in", hgt.split("in")(0).toInt)
      } else {
        Hgt("invalid", 0)
      }
      if (hgtValue.t == "invalid") {
        return false
      }
      if (hgtValue.t == "cm" && (hgtValue.value < 150 || hgtValue.value > 193)) {
        return false
      }
      if (hgtValue.t == "in" && (hgtValue.value < 59 || hgtValue.value > 76)) {
        return false
      }

      val hcl = hclMaybe.get.value
      if (!hcl.matches("#[0-9a-f]{6}")) {
        return false
      }

      val ecl = eclMaybe.get.value
      if (ecl != "amb" && ecl != "blu" && ecl != "brn" && ecl != "gry" && ecl != "grn" && ecl != "hzl" && ecl != "oth") {
        return false
      }

      val pid = pidMaybe.get.value
      if (!pid.matches("\\d{9}")) {
        return false
      }

      true
    } else {
      false
    }
  }

  val parsedDocuments = inputDocuments.map { document =>
    val splitFields = document.replace(" ", "!!").split("!!")

    val parsedFields = splitFields.map(parseField)

    checkDocument(parsedFields)
  }

  println(parsedDocuments.count(_ == true))
}
