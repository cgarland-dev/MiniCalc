package minicalc.ast

/**
 * Binary operator definitions for MiniCalc.
 * 
 * Operators are grouped by category for type-safe evaluation:
 * - ArithmeticOp: Takes numbers, returns number
 * - ComparisonOp: Takes numbers, returns boolean
 * - LogicalOp: Takes booleans, returns boolean
 */

/** Base trait for all binary operators */
sealed trait BinaryOp

// ARITHMETIC OPERATORS
/** Operators that work on numbers and return numbers */
sealed trait ArithmeticOp extends BinaryOp

case object Add extends ArithmeticOp  // +
case object Sub extends ArithmeticOp  // -
case object Div extends ArithmeticOp  // /
case object Mul extends ArithmeticOp  // *

// COMPARISON OPERATORS
/** Operators that compare numbers and return booleans */
sealed trait ComparisonOp extends BinaryOp

case object Lt extends ComparisonOp   // <
case object Gt extends ComparisonOp   // >
case object Lte extends ComparisonOp  // <=
case object Gte extends ComparisonOp  // >=
case object Eq extends ComparisonOp   // ==
case object Neq extends ComparisonOp  // !=

// LOGICAL OPERATORS
/** Operators that work on booleans and return booleans */
sealed trait LogicalOp extends BinaryOp

case object And extends LogicalOp  // &&
case object Or extends LogicalOp   // ||

// HELPER UTILITIES
/** Helper object for converting string operators to BinaryOp types */
object Op {
  /** Maps operator symbols to their corresponding BinaryOp instances */
  val stringToOp: Map[String, BinaryOp] = Map(
    "+"  -> Add,
    "-"  -> Sub,
    "/"  -> Div,
    "*"  -> Mul,
    "<"  -> Lt,
    ">"  -> Gt, 
    "<=" -> Lte, 
    ">=" -> Gte,
    "==" -> Eq,
    "!=" -> Neq,
    "&&" -> And,
    "||" -> Or
  )
}