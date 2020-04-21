package polishcalc



//  sealed trait BinaryOperation{
//    final val SumSign = '+'
//    final val SubtractSign = '-'
//    final val MultiplySign = '*'
//    final val DevideSign = '/'
//    final val LParenthesesSign = '('
//    final val RParenthesesSign = ')'
//  }



object PolishCalculator {

  final val SumSign = '+'
  final val SubtractSign = '-'
  final val MultiplySign = '*'
  final val DevideSign = '/'
  final val LParenthesesSign = '('
  final val RParenthesesSign = ')'

  def formatString(s: String) = {
    s.filterNot((x: Char) => x.isWhitespace)
  }

  sealed trait Term //class trait, if i need constructor arguments
  case class ElementaryTerm(leftOperand: Int, rightOperand: Int, operation: BinaryOperation) extends Term()
  case class NonElementaryTerm(leftOperand: Term, rightOperand: Term, operation: BinaryOperation) extends Term()

  sealed abstract class BinaryOperation(val sign: Char){
    def operate(lOperand: Int, rOperand: Int): Int
  }

  private final case class SumOperation() extends BinaryOperation(sign = SumSign){
    override def operate(lOperand: Int, rOperand: Int): Int = {
      lOperand + rOperand
    }
  }

  private final case class SubtractOperation() extends BinaryOperation(sign = SubtractSign){
    override def operate(lOperand: Int, rOperand: Int): Int = {
      lOperand - rOperand
    }
  }

  private final case class MultiplyOperation() extends BinaryOperation(sign = MultiplySign) {
    override def operate(lOperand: Int, rOperand: Int): Int = {
      lOperand * rOperand
    }
  }

  private final case class DevideOperation() extends BinaryOperation(sign = DevideSign) {
    override def operate(lOperand: Int, rOperand: Int): Int = {
      lOperand / rOperand
    }
  }

  def evaluateTerm(term: Term): Int = {//how to tailrec?? operate not evaluate
    term match {
      case ElementaryTerm(l,r,bop) => bop.operate(l,r)
      case NonElementaryTerm(l,r,bop) => bop.operate(evaluateTerm(l), evaluateTerm(r))
    }
  }


  def getTermFromString() = {

  }

  def parseTerm(toParse: String, termAcc: Term) = {





    @scala.annotation.tailrec
    def getLeftTerm(toParse: String, acc: String = "", openParenthesesNum: Int = 0): String = {
//      val symbol = toParse.trim.reverse.head
      val symbol = toParse.head
      println("term="+symbol)

      symbol match {
        case LParenthesesSign =>
          println("toParse.tail="+toParse.tail)
          getLeftTerm(toParse.tail, acc.appended(symbol), openParenthesesNum+1)
        case RParenthesesSign =>
          if(openParenthesesNum - 1 == 0){
            ((10 - 5) - ((5 +5) * (4 + 4))) * 8
            acc.appended(RParenthesesSign)
          } else {
            getLeftTerm(toParse.tail, acc.appended(symbol), openParenthesesNum - 1)
          }
        case _ =>
          println("toParse.tail="+toParse.tail)
          getLeftTerm(toParse.tail, acc.appended(symbol), openParenthesesNum)
      }
    }

    val operation = toParse.reverse.head

    val startingTerm = toParse.head match {
      case SumSign => throw new Exception("Sum symbol in incorrect position")
      case SubtractSign => throw new Exception("Subtract symbol in incorrect position")
      case MultiplySign => throw new Exception("Multiply symbol in incorrect position")
      case DevideSign => throw new Exception("Devide symbol in incorrect position")
      case LParenthesesSign => getLeftTerm(toParse)
      case RParenthesesSign => throw new Exception("right parentheses is in unexpected place")
      case _ => if(operation.isDigit){"is digit"} else {"not digit"}
    }

    def getFirstTermFromLeft(s: String) = {
      println("getFirstTermFromLeft")
      (getLeftTerm(s).tail).reverse.tail
    }

    getFirstTermFromLeft(formatString(toParse))
  }






// 5 6 + 7 -      4
// 3 2 * 5 +    11
// 3 2 * 3 3 * + 5 + 20
//

//      ((10 - 5) - ((5 +5) * (4 + 4))) * 8

//  ((10 - 5) - ((5 +5) * (4 + 4))) 8 *
//  (10 - 5) ((5 +5) * (4 + 4)) - 8 *
//    (10 - 5) ((5 5 +) * (4 4 +) - 8 *
//    (10 - 5) (5 5 + 4 4 + *) - 8 *
//    (10 5 -) (5 5 + 4 4 + *) - 8 *

//  10 5 - 5 5 + 4 4 + * - 8 *
//  ((10 - 5) - (5 + 5) * (4 + 4)) * 8
//  4 * 4 * 4


  def main(args: Array[String]) {
    val src = scala.io.StdIn.readLine().trim
    println("src="+src)

    val x = parseTerm(src)
    println(x)
  }


}
