package days_2021

import days_2021.CaveTypes.Middle

import scala.io.BufferedSource

class Cave(var name: String, var caveType: CaveTypes) {
  var nextCaves: Seq[Cave] = Seq()
  var visited: Boolean = false

  def isBig: Boolean = {
    name.toUpperCase() == name
  }

  def getNextCaves: Seq[Cave] = {
    nextCaves
  }

  def addNextCaves(cave: Cave): Unit = {
    nextCaves = nextCaves.appended(cave)
  }

  def setVisited(value: Boolean): Unit = {
    visited = value
  }
}

class ResultPaths(result: Seq[String]) {
}

object Day_12 {

  def solveCaves(caves: Seq[Cave], cave: Cave, path: String): Long = {
    if cave.caveType == CaveTypes.End then {
      println(path + cave.name + " ---- FINISHED")
      return 1L
    }
    if !cave.isBig && cave.visited then {
      println(path + cave.name)
      return 0L
    }
    cave.setVisited(true)
    var numberOfPaths = 0L
    cave.getNextCaves.foreach(nextCave => numberOfPaths = numberOfPaths + solveCaves(caves, nextCave, path + cave.name + ","))
    cave.setVisited(false)
    numberOfPaths
  }

  // better solution is with Set
  def parseLine(caves: Seq[Cave], line: String): Seq[Cave] = {
    var cavesCopy = caves
    val caveStrings = line.split('-')
    val caveFirstName = caveStrings(0).trim()
    val caveSecondName = caveStrings(1).trim()
    val containsFirst = caves.exists(c => c.name == caveFirstName)
    val containsSecond = caves.exists(c => c.name == caveSecondName)
    val first = if containsFirst then caves.find(c => c.name == caveFirstName).get else Cave(caveFirstName, parseCaveType(caveFirstName))
    val second = if containsSecond then caves.find(c => c.name == caveSecondName).get else Cave(caveSecondName, parseCaveType(caveSecondName))
    first.addNextCaves(second)
    second.addNextCaves(first)
    if !containsFirst then cavesCopy = cavesCopy.appended(first)
    if !containsSecond then cavesCopy = cavesCopy.appended(second)
    cavesCopy
  }

  def parseCaveType(caveName: String): CaveTypes = {
    caveName match
      case "start" => CaveTypes.Start
      case "end" => CaveTypes.End
      case _ => CaveTypes.Middle
  }

  def run(): Unit = {
    val lines1 = Array("start - A",
      "start-b",
      "A-c",
      "A-b",
      "b-d",
      "A-end",
      "b-end")
    val lines2 = Array("dc - end",
      "HN -start",
      "start -kj",
      "dc -start",
      "dc -HN",
      "LN -dc",
      "HN -end",
      "kj -sa",
      "kj -HN",
      "kj -dc")
    val lines3 = Array("fs - end",
      "he -DX",
      "fs -he",
      "start -DX",
      "pj -DX",
      "end -zg",
      "zg -sl",
      "zg -pj",
      "pj -he",
      "RW -he",
      "fs -DX",
      "pj -RW",
      "zg -RW",
      "start -pj",
      "he -WI",
      "zg -he",
      "pj -fs",
      "start -RW")

    val source: BufferedSource = scala.io.Source.fromFile("day12_input1.txt")
    val linesFinal: Array[String] = try source.mkString.split("\n") finally source.close()
    var caves: Seq[Cave] = Seq()
    linesFinal.foreach(line => caves = parseLine(caves, line))
    var resultPaths = ResultPaths(Seq())
    val startingCave = caves.find(cave => cave.caveType == CaveTypes.Start).get
    println(solveCaves(caves, startingCave, ""))
  }

}

enum CaveTypes:
  case Start, End, Middle
