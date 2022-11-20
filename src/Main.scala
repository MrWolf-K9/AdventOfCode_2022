import scala.io.BufferedSource

object Main {
  val source: BufferedSource = scala.io.Source.fromFile("day9_input1.txt")
  val lines: Array[String] = try source.mkString.split("\n") finally source.close()
  //val lines: Array[String] = Array("2199943210", "3987894921", "9856789892", "8767896789", "9899965678")
  val maxX: Int = lines(0).length
  val maxY: Int = lines.length

  def getPoint(x: Int, y: Int): Int = {
    if (x < 0 || x >= maxX) || (y < 0 || y >= maxY) then return 10
    lines(y)(x).asDigit
  }

  def isLowPoint(x: Int, y: Int, value: Int): Boolean = {
    value < getPoint(x + 1, y) && value < getPoint(x, y + 1) && value < getPoint(x - 1, y) && value < getPoint(x, y - 1)
  }

  def main(args: Array[String]): Unit = {
    println(
      lines.zipWithIndex.map(
        (line, indexY) => line.zipWithIndex.flatMap(
          (char, indexX) =>
            if isLowPoint(indexX, indexY, char.toString.toInt)
            then Iterable(getPoint(indexX, indexY) + 1)
            else Iterable(0)
        ).sum
      ).sum
    )
  }
}