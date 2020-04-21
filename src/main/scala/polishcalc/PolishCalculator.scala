package polishcalc

object PolishCalculator {

  final val SumSign = '+'
  final val SubtractSign = '-'
  final val MultiplySign = '*'
  final val DevideSign = '/'
  final val LParenthesesSign = '('
  final val RParenthesesSign = ')'
  final val MinPriority = 1

  def formatString(s: String) = {
    s.filterNot((x: Char) => x.isWhitespace)
  }

  sealed trait Term
//  final case class ElementaryOperationTerm(leftOperand: NumberTerm, rightOperand: NumberTerm, operation: BinaryOperation) extends Term
//  final case class NonElementaryTerm(terms: Seq[Term], operations: Seq[BinaryOperation]) extends Term
//  final case class NumberTerm(number: Int) extends Term

  final case class OperationTerm(leftOperand: Term, rightOperand: Term, operation: BinaryOperation) extends Term
  final case class NonElementaryTerm(terms: Seq[Term], operations: Seq[BinaryOperation]) extends Term
  final case class NumberTerm(number: Int) extends Term


  sealed abstract class BinaryOperation(val sign: Char, val priority: Int) {
    def operate(lOperand: Int, rOperand: Int): Int
  }

  final case class SumOperation() extends BinaryOperation(sign = SumSign, priority = 1) {
    override def operate(lOperand: Int, rOperand: Int): Int = {
      lOperand + rOperand
    }
  }

  final case class SubtractOperation() extends BinaryOperation(sign = SubtractSign, priority = 1) {
    override def operate(lOperand: Int, rOperand: Int): Int = {
      lOperand - rOperand
    }
  }

  private final case class MultiplyOperation() extends BinaryOperation(sign = MultiplySign, priority = 0) {
    override def operate(lOperand: Int, rOperand: Int): Int = {
      lOperand * rOperand
    }
  }

  private final case class DevideOperation() extends BinaryOperation(sign = DevideSign,  priority = 0) {
    override def operate(lOperand: Int, rOperand: Int): Int = {
      lOperand / rOperand
    }
  }

  def evaluateTerm(term: Term): Int = {
    term match {
      case ElementaryOperationTerm(l,r,bop) => bop.operate(l.number, r.number)
//      case NonElementaryTerm(l,r,bop) => bop.operate(evaluateTerm(l), evaluateTerm(r))
      case NonElementaryTerm(terms,ops) =>
    }
  }









  @scala.annotation.tailrec
  def getTermFromSeq(terms: Seq[Term], ops: Seq[BinaryOperation], priority: Int = MinPriority, acc: Seq[Term]): Seq[Term] = {

    @scala.annotation.tailrec
    def getTermsSeqForPriority(
                                termsSeq: Seq[Term],
                                opsSeq: Seq[BinaryOperation],
                                previousTerm: Term,
                                currentPriority: Int,
                                termsAcc: Seq[Term] = Seq.empty[Term],
                                opsAcc: Seq[BinaryOperation] = Seq.empty[BinaryOperation]): (Seq[Term], Seq[BinaryOperation]) = {
      opsSeq match {
        case op :: Nil => (termsAcc, opsAcc)
        case op :: tail => if(op.priority == currentPriority) {



          val newTerm = NonElementaryTerm(previousTerm, terms.head, op)
          getTermsSeqForPriority(termsSeq.tail, tail, termsAcc.appended(NonElementaryTerm(previousTerm, terms.head, op)))


        } else {
          getTermsSeqForPriority(termsSeq.tail, tail, termsAcc.appended(terms.head), opsAcc.appended(ops.tail), currentPriority)
        }
      }
    }
    val termsAndOpsOnPriotityLevel = getTermsSeqForPriority(terms.tail, ops, terms.head, 0)
    if(priority > MinPriority) acc else getTermFromSeq(termsAndOpsOnPriotityLevel._1, termsAndOpsOnPriotityLevel._2, priority + 1, acc)
  }

  def parseTerm(
                 toParse: String,
                 termsAcc: Seq[Term] = Seq.empty[Term],
                 operationsAcc: Seq[BinaryOperation] = Seq.empty[BinaryOperation],
                 numberAcc: String,
                 acc: Term = NumberTerm(0)): Term = {//dont forget 0 is here!!!!

    toParse.head match {
      case c if(c.isDigit) => parseTerm(toParse.tail, termsAcc, operationsAcc, numberAcc.appended(c), acc)
      case SumSign =>
        if(numberAcc.length > 0){//!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!! DRY!!!
          val numToCache = numberAcc.toInt
          parseTerm(toParse.tail, termsAcc.appended(NumberTerm(numToCache)), operationsAcc.appended(SumOperation), "", acc)//add digit tio cache and operation to remember
        } else {
          throw new Exception("Operation symbol is in incorrect position")
        }
      case SubtractSign => //DRY!!!
        if(numberAcc.length > 0){//!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!! DRY!!!
          val numToCache = numberAcc.toInt
          parseTerm(toParse.tail, termsAcc.appended(NumberTerm(numToCache)), operationsAcc.appended(SubtractOperation), "", acc)//add digit tio cache and operation to remember
        } else {
          throw new Exception("Operation symbol is in incorrect position")
        }
      case MultiplySign =>
        if(numberAcc.length > 0){//!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!! DRY!!!
          val numToCache = numberAcc.toInt
          parseTerm(toParse.tail, termsAcc.appended(NumberTerm(numToCache)), operationsAcc.appended(MultiplyOperation), "", acc)//add digit tio cache and operation to remember
        } else {
          throw new Exception("Operation symbol is in incorrect position")
        }
      case DevideSign =>
        if(numberAcc.length > 0){//!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!! DRY!!!
          val numToCache = numberAcc.toInt
          parseTerm(toParse.tail, termsAcc.appended(NumberTerm(numToCache)), operationsAcc.appended(DevideOperation), "", acc)//add digit tio cache and operation to remember
        } else {
          throw new Exception("Operation symbol is in incorrect position")
        }
      case LParenthesesSign => parseTerm(getTermStringInParentheses(toParse.tail))
    }

    @scala.annotation.tailrec
    def getTermStringInParentheses(toParse: String, acc: String, openParenthesesNum: Int = 0): String = {
      val symbol = toParse.head
      symbol match {
        case LParenthesesSign =>
          getTermStringInParentheses(toParse.tail, acc.appended(symbol), openParenthesesNum+1)
        case RParenthesesSign =>
          if(openParenthesesNum - 1 == 0)acc.appended(RParenthesesSign) else getTermStringInParentheses(toParse.tail, acc.appended(symbol), openParenthesesNum - 1)
        case _ => getTermStringInParentheses(toParse.tail, acc.appended(symbol), openParenthesesNum)
      }
    }


Option

//    val operation = toParse.reverse.head

//    def getFirstTermFromLeft(s: String) = {
//      println("getFirstTermFromLeft")
//      (getTermInParentheses(s).tail).reverse.tail
//    }

    getFirstTermFromLeft(formatString(toParse))
  }


5 5 7 4 3 4
* - + / -
//
//  term(5,5,*)7434
//  -+/-
//
//  term(5,5,*) term(7) term(4 3 /) term(3,4,/)
//  - + -
//
//
//
//
//  5574349
//  -*-+/-



//5 * 5 - 7 + 4 / 3 - 4

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
