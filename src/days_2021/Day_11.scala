package days_2021

import scala.annotation.tailrec
import scala.util.control.Breaks._

object Day_11 {
  var flashes = 0
  var step = 0
  val lines: Array[String] = Array(
    "8258741254",
    "3335286211",
    "8468661311",
    "6164578353",
    "2138414553",
    "1785385447",
    "3441133751",
    "3586862837",
    "7568272878",
    "6833643144")
  val linesTest: Array[String] = Array(
    "5483143223",
    "2745854711",
    "5264556173",
    "6141336146",
    "6357385478",
    "4167524645",
    "2176841721",
    "6882881134",
    "4846848554",
    "5283751526")

  def hasFlash(field: Array[String]): Boolean = {
    field.map(x => x.contains('+')).reduce((a, b) => a || b)
  }

  def increaseOctopus(chr: Char): Char = {
    if chr == '+' || chr == '-' then return chr
    val number = chr.toString.toInt
    number match
      case 9 =>
        flashes = flashes + 1
        '+'
      case x => ('0' + (x + 1)).toChar
  }

  def updateRound(array: Array[String], rounds: Int): Array[String] = {
    var round = array
    0 until rounds foreach (_ => {
      round = round.map(line => line.map(octopus => increaseOctopus(octopus)))
      round = update(round).map(line => line.map(chr => if chr == '-' then '0' else chr))
      println(round.mkString("Array(", ", ", ")"))
      step = step + 1
      if round.map(line => line.map(chr => chr == '0').reduce((a, b) => a && b))
        .reduce((a, b) => a && b) then
        println("ALL FLASHES at " + step.toString)
        break()
    })
    round
  }

  @tailrec
  def update(array: Array[String]): Array[String] = {
    if !hasFlash(array) then return array
    var result = array
    result.indices foreach (indexY =>
      result(indexY).indices foreach (indexX =>
        if result(indexY)(indexX) == '+' then {
          result = increaseAdjacent(result, indexX, indexY)
          result = result.updated(indexY, result(indexY).updated(indexX, '-'))
        }
        )
      )
    update(result)
  }

  def increaseAdjacent(array: Array[String], x: Int, y: Int): Array[String] = {
    var result = array
    result = increasePosition(result, x + 1, y)
    result = increasePosition(result, x - 1, y)
    result = increasePosition(result, x, y + 1)
    result = increasePosition(result, x, y - 1)


    result = increasePosition(result, x + 1, y + 1)
    result = increasePosition(result, x - 1, y - 1)
    result = increasePosition(result, x - 1, y + 1)
    result = increasePosition(result, x + 1, y - 1)
    result
  }

  def increasePosition(array: Array[String], x: Int, y: Int): Array[String] = {
    (x, y) match
      case (x, y) if x >= 0
        && x < array(0).length
        && y >= 0
        && y < array.length
      => array.updated(y, array(y).updated(x, increaseOctopus(array(y)(x))))
      case (_, _) => array
  }

  def run(): Unit = {
    updateRound(lines, 2000)
    println(flashes)
  }
}
