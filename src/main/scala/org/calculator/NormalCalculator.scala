package org.calculator

import scala.util.parsing.combinator._
import scala.io.StdIn
  /*
trait Maths {
  def add(x: Float, y: Float) = x + y
  def sub(x: Float, y: Float) = x - y
  def mul(x: Float, y: Float) = x * y
  def div(x: Float, y: Float) = if (y > 0) (x / y) else 0.0f
} */
object NormalCalculator extends JavaTokenParsers with Maths {
  def num: Parser[Double] = floatingPointNumber ^^ {_.toDouble}
  def factor: Parser[Double] = num | "(" ~> expr <~ ")"
  def term: Parser[Double] = factor ~ rep("*" ~ factor | "/" ~ factor) ^^ {
    case number ~ list => (number /: list){
      case (x, "*" ~ f) => x * f
      case (x, "/" ~ y) => x / y
    }
  }

  def expr: Parser[Double] = term ~ rep("+" ~ term | "-" ~ term) ^^ {
    case number ~ list => (number /: list){
      case (x, "+" ~ y) => x + y
      case (x, "-" ~ y) => x - y
    }
  }

  def apply(str: String):String = parseAll(expr, str) match {
    case Success(result, next) => result.toString
    case _: NoSuccess => "error"
  }

  def main(args: Array[String]) = {

    var line:String = StdIn.readLine()
    while(line != null) {
      println(apply(line))
      line = StdIn.readLine()
    }
  }
}
