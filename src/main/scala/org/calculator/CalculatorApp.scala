package org.calculator

import scala.util.parsing.combinator._
trait Maths {
  def add(x: Float, y: Float) = x + y
  def sub(x: Float, y: Float) = x - y
  def mul(x: Float, y: Float) = x * y
  def div(x: Float, y: Float) = if (y > 0) (x / y) else 0.0f
}


object CalculatorApp extends JavaTokenParsers with Maths {

  /**
    * Allow multiple instance of the num to appear
    * @return
    */
  def term: Parser[List[Float]] = rep(num)
  /**
    * To match a number and convert them to primitives
    * @return
    */
  def num: Parser[Float] = floatingPointNumber ^^ (_.toFloat)

  /**
    * To match a operator and convert them
    * @return
    */
  def operator: Parser[(Float, Float) => Float] = ("*" | "/" | "+" | "-") ^^ {
    case "+" => (x, y) => x + y
    case "-" => (x, y) => x - y
    case "*" => (x, y) => x * y
    case "/" => (x, y) => if(y != 0) x / y else 0.floatValue()
  }

  def reduce(nums: List[Float], op: (Float, Float) => Float): List[Float] = nums match {
    case x :: y :: xs => op(y, x) :: xs
    case x :: Nil => x :: Nil
    case Nil => Nil
  }

  def expr: Parser[Float] = rep(term ~ operator) ^^ {
    terms =>
      // stack here
      var stack = List.empty[Float]

      // default operation
      var lastOp:(Float, Float) => Float = (x, y) => x + y

      terms.foreach {
        case nums ~ op =>
          lastOp = op
          // put the number on top of the stack
          stack = reduce(nums.reverse ::: stack, op)
      }

      stack.reduceLeft((y, x) => lastOp(x, y))
  }
  def main(args: Array[String]): Unit = {
    val input = "5 1 2 + 4 * 3 -"
    println(calculate(input))
  }

  def calculate(expression: String) = parseAll(expr, expression)
}
