package days_2021

import scala.annotation.tailrec
import scala.io.BufferedSource

object Day_10 {
  val lines: Array[String] = Array(
    "[({(<(())[]>[[{[]{<()<>>",
    "[(()[<>])]({[<{<<[]>>(",
    "{([(<{}[<>[]}>{[]{[(<()>",
    "(((({<>}<{<{<>}{[]{[]{}",
    "[[<[([]))<([[{}[[()]]]",
    "[{[{({}]{}}([{[{{{}}([]",
    "{<[[]]>}<{[{[{[]{()[[[]",
    "[<(<(<(<{}))><([]([]()",
    "<{([([[(<>()){}]>(<<{{",
    "<{([{{}}[<[[[<>{}]]]>[]]")


  //val source: BufferedSource = scala.io.Source.fromFile("day10_input1.txt")
  //val lines: Array[String] = try source.mkString.split("\n") finally source.close()

  def handleChar(stack: Array[Char], chr: Char): Either[Array[Char], Int] = {
    var stackCopy = stack
    chr match
      case chr if chr == '(' || chr == '[' || chr == '{' || chr == '<' => stackCopy = stack :+ chr
      case chr => if isClosing(stackCopy.last, chr) then stackCopy = stackCopy.init else return Right(getScore(chr))
    Left(stackCopy)
  }

  def getScore(chr: Char): Int = {
    chr match
      case ')' => 3
      case ']' => 57
      case '}' => 1197
      case '>' => 25137
  }

  def isClosing(start: Char, close: Char): Boolean = {
    (start, close) match
      case ('(', ')') => true
      case ('[', ']') => true
      case ('{', '}') => true
      case ('<', '>') => true
      case (_, _) => false
  }


  def solveStack(stack: Array[Char]): Int = {
    val gradedStack = stack.reverse.map(chr => chr match
      case '(' => 1
      case '[' => 2
      case '{' => 3
      case '<' => 4
    )
    gradedStack.fold(0)((a, b) => a * 5 + b)
  }

  @tailrec
  def handleLine(array: Array[Char], stack: Array[Char], index: Int): Int = {
    if index >= array.length then return 0
    val result = handleChar(stack, array(index))
    result match
      case Left(value) => handleLine(array, value, index + 1)
      case Right(value) => value
  }

  @tailrec
  def handleLineFinish(array: Array[Char], stack: Array[Char], index: Int): Int = {
    if index >= array.length then
      if !stack.isEmpty then return solveStack(stack) else return 0
    val result = handleChar(stack, array(index))
    result match
      case Left(value) => handleLineFinish(array, value, index + 1)
      case Right(_) => 0
  }

  def run(): Unit = {
    val res = lines.map(line => handleLineFinish(line.toCharArray, Array(), 0)).filter(x => x != 0)
    println(res.sorted(Ordering.Int)(res.length / 2))
  }

}
