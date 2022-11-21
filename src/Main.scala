import scala.annotation.unused
import scala.io.BufferedSource

object Main {
  val source: BufferedSource = scala.io.Source.fromFile("day9_input1.txt")
  val lines: Array[String] = try source.mkString.split("\n") finally source.close()
  //val lines: Array[String] = Array("2199943210", "3987894921", "9856789892", "8767896789", "9899965678")
  val maxX: Int = lines(0).length
  val maxY: Int = lines.length
  val visitedMatrix: Array[Array[Int]] = Array.ofDim[Int](maxY, maxX)
  val visitedMatrixInit: Array[Array[Int]] = visitedMatrix.map(line => line.map(_ => 0))

  def getPoint(x: Int, y: Int): Int = {
    if (x < 0 || x >= maxX) || (y < 0 || y >= maxY) then return 9
    lines(y)(x).asDigit
  }

  def setVisited(x: Int, y: Int): Unit = {
    if !(x < 0 || x >= maxX) || (y < 0 || y >= maxY) then
      visitedMatrixInit(y)(x) = 1
  }

  def isVisited(x: Int, y: Int): Boolean = {
    visitedMatrixInit(y)(x) == 1
  }

  def isLowPoint(x: Int, y: Int, value: Int): Boolean = {
    value < getPoint(x + 1, y) && value < getPoint(x, y + 1) && value < getPoint(x - 1, y) && value < getPoint(x, y - 1)
  }

  def countBasin(x: Int, y: Int, prev: Int): Int = {
    val point = getPoint(x, y)
    if point == 9 || isVisited(x, y) || point <= prev then return 0
    //println(point)
    setVisited(x, y)
    point match
      case value => 1 + countBasin(x, y + 1, value)
        + countBasin(x, y - 1, value)
        + countBasin(x + 1, y, value)
        + countBasin(x - 1, y, value)
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
    val monsins: Array[Int] = lines.zipWithIndex.map(
        (line, indexY) => line.zipWithIndex.flatMap(
          (char, indexX) =>
            if isLowPoint(indexX, indexY, char.toString.toInt)
            then Iterable(countBasin(indexX, indexY, -1))
            else Iterable(0)
        )
      ).reduce((acc, array) => acc ++ array).toArray

    println(monsins.sorted(Ordering[Int].reverse).take(3).product)
    println("Finished")
  }
}