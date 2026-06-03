package minicalc.parser

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import org.scalatest.EitherValues

import minicalc.ast._
import minicalc.eval.ParseError

/**
 * Comprehensive tests for the MiniCalc parser.
 *
 * Tests cover:
 * - Literal parsing (numbers, booleans)
 * - Variable parsing
 * - Binary operations with precedence
 * - Let bindings
 * - Conditionals
 * - Task definitions with all modifiers
 * - Task execution expressions
 * - Sequence and parallel expressions
 * - Schedule expressions
 * - Workflow definitions
 * - Error handling with position tracking
 */
class ParserSpec extends AnyFlatSpec with Matchers with EitherValues {

  // ============================================
  // LITERAL PARSING
  // ============================================

  "Parser" should "parse integer literals" in {
    Parser.parse("42") shouldBe Right(NumLit(42.0))
    Parser.parse("0") shouldBe Right(NumLit(0.0))
    Parser.parse("123456") shouldBe Right(NumLit(123456.0))
  }

  it should "parse decimal literals" in {
    Parser.parse("3.14") shouldBe Right(NumLit(3.14))
    Parser.parse("0.5") shouldBe Right(NumLit(0.5))
    Parser.parse("123.456") shouldBe Right(NumLit(123.456))
  }

  it should "parse boolean literals" in {
    Parser.parse("true") shouldBe Right(BoolLit(true))
    Parser.parse("false") shouldBe Right(BoolLit(false))
  }

  // ============================================
  // VARIABLE PARSING
  // ============================================

  it should "parse simple variable names" in {
    Parser.parse("x") shouldBe Right(Var("x"))
    Parser.parse("foo") shouldBe Right(Var("foo"))
    Parser.parse("myVar") shouldBe Right(Var("myVar"))
  }

  it should "parse variable names with underscores and numbers" in {
    Parser.parse("my_var") shouldBe Right(Var("my_var"))
    Parser.parse("var1") shouldBe Right(Var("var1"))
    Parser.parse("foo_bar_123") shouldBe Right(Var("foo_bar_123"))
  }

  // ============================================
  // ARITHMETIC OPERATIONS
  // ============================================

  it should "parse addition" in {
    Parser.parse("1 + 2") shouldBe Right(BinOp(Add, NumLit(1), NumLit(2)))
  }

  it should "parse subtraction" in {
    Parser.parse("5 - 3") shouldBe Right(BinOp(Sub, NumLit(5), NumLit(3)))
  }

  it should "parse multiplication" in {
    Parser.parse("4 * 2") shouldBe Right(BinOp(Mul, NumLit(4), NumLit(2)))
  }

  it should "parse division" in {
    Parser.parse("10 / 2") shouldBe Right(BinOp(Div, NumLit(10), NumLit(2)))
  }

  it should "handle operator precedence (* before +)" in {
    // 1 + 2 * 3 should parse as 1 + (2 * 3)
    Parser.parse("1 + 2 * 3") shouldBe Right(
      BinOp(Add, NumLit(1), BinOp(Mul, NumLit(2), NumLit(3)))
    )
  }

  it should "handle operator precedence (/ before -)" in {
    // 10 - 6 / 2 should parse as 10 - (6 / 2)
    Parser.parse("10 - 6 / 2") shouldBe Right(
      BinOp(Sub, NumLit(10), BinOp(Div, NumLit(6), NumLit(2)))
    )
  }

  it should "handle left-to-right associativity" in {
    // 1 - 2 - 3 should parse as (1 - 2) - 3
    Parser.parse("1 - 2 - 3") shouldBe Right(
      BinOp(Sub, BinOp(Sub, NumLit(1), NumLit(2)), NumLit(3))
    )
  }

  it should "handle parentheses for grouping" in {
    // (1 + 2) * 3 should parse with grouping
    Parser.parse("(1 + 2) * 3") shouldBe Right(
      BinOp(Mul, BinOp(Add, NumLit(1), NumLit(2)), NumLit(3))
    )
  }

  // ============================================
  // COMPARISON OPERATIONS
  // ============================================

  it should "parse less than" in {
    Parser.parse("1 < 2") shouldBe Right(BinOp(Lt, NumLit(1), NumLit(2)))
  }

  it should "parse greater than" in {
    Parser.parse("5 > 3") shouldBe Right(BinOp(Gt, NumLit(5), NumLit(3)))
  }

  it should "parse less than or equal" in {
    Parser.parse("1 <= 2") shouldBe Right(BinOp(Lte, NumLit(1), NumLit(2)))
  }

  it should "parse greater than or equal" in {
    Parser.parse("5 >= 3") shouldBe Right(BinOp(Gte, NumLit(5), NumLit(3)))
  }

  it should "parse equality" in {
    Parser.parse("1 == 1") shouldBe Right(BinOp(Eq, NumLit(1), NumLit(1)))
  }

  it should "parse inequality" in {
    Parser.parse("1 != 2") shouldBe Right(BinOp(Neq, NumLit(1), NumLit(2)))
  }

  // ============================================
  // LOGICAL OPERATIONS
  // ============================================

  it should "parse logical AND" in {
    Parser.parse("true && false") shouldBe Right(BinOp(And, BoolLit(true), BoolLit(false)))
  }

  it should "parse logical OR" in {
    Parser.parse("true || false") shouldBe Right(BinOp(Or, BoolLit(true), BoolLit(false)))
  }

  it should "handle logical operator precedence (&& before ||)" in {
    // true || false && true should parse as true || (false && true)
    Parser.parse("true || false && true") shouldBe Right(
      BinOp(Or, BoolLit(true), BinOp(And, BoolLit(false), BoolLit(true)))
    )
  }

  it should "handle comparison before logical operators" in {
    // 1 < 2 && 3 > 2 should parse correctly
    Parser.parse("1 < 2 && 3 > 2") shouldBe Right(
      BinOp(And, BinOp(Lt, NumLit(1), NumLit(2)), BinOp(Gt, NumLit(3), NumLit(2)))
    )
  }

  // ============================================
  // LET BINDINGS
  // ============================================

  it should "parse simple let binding" in {
    Parser.parse("let x = 5 in x") shouldBe Right(
      Let("x", NumLit(5), Var("x"))
    )
  }

  it should "parse let binding with expression body" in {
    Parser.parse("let x = 5 in x + 1") shouldBe Right(
      Let("x", NumLit(5), BinOp(Add, Var("x"), NumLit(1)))
    )
  }

  it should "parse nested let bindings" in {
    Parser.parse("let x = 1 in let y = 2 in x + y") shouldBe Right(
      Let("x", NumLit(1), Let("y", NumLit(2), BinOp(Add, Var("x"), Var("y"))))
    )
  }

  it should "parse let binding with complex value expression" in {
    Parser.parse("let x = 1 + 2 in x * 3") shouldBe Right(
      Let("x", BinOp(Add, NumLit(1), NumLit(2)), BinOp(Mul, Var("x"), NumLit(3)))
    )
  }

  // ============================================
  // CONDITIONALS
  // ============================================

  it should "parse simple if-then-else" in {
    Parser.parse("if true then 1 else 2") shouldBe Right(
      If(BoolLit(true), NumLit(1), NumLit(2))
    )
  }

  it should "parse if with comparison condition" in {
    Parser.parse("if x > 0 then x else 0") shouldBe Right(
      If(BinOp(Gt, Var("x"), NumLit(0)), Var("x"), NumLit(0))
    )
  }

  it should "parse nested conditionals" in {
    Parser.parse("if true then if false then 1 else 2 else 3") shouldBe Right(
      If(BoolLit(true), If(BoolLit(false), NumLit(1), NumLit(2)), NumLit(3))
    )
  }

  // ============================================
  // TASK DEFINITIONS
  // ============================================

  it should "parse simple task definition" in {
    val result = Parser.parse("define task myTask = 42")
    result.isRight shouldBe true
    result.value match {
      case TaskDef(name, body, priority, deps, timeout, retry) =>
        name shouldBe "myTask"
        body shouldBe NumLit(42)
        priority shouldBe None
        deps shouldBe List.empty
        timeout shouldBe None
        retry shouldBe None
      case _ => fail("Expected TaskDef")
    }
  }

  it should "parse task definition with priority" in {
    val result = Parser.parse("define task myTask = 42 with priority High")
    result.isRight shouldBe true
    result.value match {
      case TaskDef(name, _, priority, _, _, _) =>
        name shouldBe "myTask"
        priority shouldBe Some(Var("High"))
      case _ => fail("Expected TaskDef")
    }
  }

  it should "parse task definition with dependencies" in {
    val result = Parser.parse("define task myTask = 42 depends on task1, task2")
    result.isRight shouldBe true
    result.value match {
      case TaskDef(_, _, _, deps, _, _) =>
        deps shouldBe List("task1", "task2")
      case _ => fail("Expected TaskDef")
    }
  }

  it should "parse task definition with timeout" in {
    val result = Parser.parse("define task myTask = 42 timeout 30")
    result.isRight shouldBe true
    result.value match {
      case TaskDef(_, _, _, _, timeout, _) =>
        timeout shouldBe Some(NumLit(30))
      case _ => fail("Expected TaskDef")
    }
  }

  it should "parse task definition with linear retry" in {
    val result = Parser.parse("define task myTask = 42 retry 3 backoff 5")
    result.isRight shouldBe true
    result.value match {
      case TaskDef(_, _, _, _, _, retry) =>
        retry shouldBe Some(RetryConfig(3, 5.0, false))
      case _ => fail("Expected TaskDef")
    }
  }

  it should "parse task definition with exponential retry" in {
    val result = Parser.parse("define task myTask = 42 retry 3 backoff 5 exponential = true")
    result.isRight shouldBe true
    result.value match {
      case TaskDef(_, _, _, _, _, retry) =>
        retry shouldBe Some(RetryConfig(3, 5.0, true))
      case _ => fail("Expected TaskDef")
    }
  }

  it should "parse task definition with all modifiers" in {
    val result = Parser.parse(
      "define task myTask = 42 with priority High depends on dep1, dep2 timeout 30 retry 3 backoff 5"
    )
    result.isRight shouldBe true
    result.value match {
      case TaskDef(name, body, priority, deps, timeout, retry) =>
        name shouldBe "myTask"
        body shouldBe NumLit(42)
        priority shouldBe Some(Var("High"))
        deps shouldBe List("dep1", "dep2")
        timeout shouldBe Some(NumLit(30))
        retry shouldBe Some(RetryConfig(3, 5.0, false))
      case _ => fail("Expected TaskDef")
    }
  }

  // ============================================
  // TASK EXECUTION
  // ============================================

  it should "parse task execution" in {
    Parser.parse("execute myTask") shouldBe Right(TaskExec("myTask"))
  }

  // ============================================
  // SEQUENCE AND PARALLEL
  // ============================================

  it should "parse sequence expression" in {
    val result = Parser.parse("sequence [execute t1, execute t2]")
    result.isRight shouldBe true
    result.value match {
      case TaskSequence(tasks) =>
        tasks shouldBe List(TaskExec("t1"), TaskExec("t2"))
      case _ => fail("Expected TaskSequence")
    }
  }

  it should "parse parallel expression" in {
    val result = Parser.parse("parallel [execute t1, execute t2, execute t3]")
    result.isRight shouldBe true
    result.value match {
      case TaskParallel(tasks) =>
        tasks shouldBe List(TaskExec("t1"), TaskExec("t2"), TaskExec("t3"))
      case _ => fail("Expected TaskParallel")
    }
  }

  it should "parse empty sequence" in {
    val result = Parser.parse("sequence []")
    result.isRight shouldBe true
    result.value match {
      case TaskSequence(tasks) =>
        tasks shouldBe List.empty
      case _ => fail("Expected TaskSequence")
    }
  }

  // ============================================
  // SCHEDULE EXPRESSIONS
  // ============================================

  it should "parse schedule with FIFO strategy" in {
    val result = Parser.parse("schedule sequence [execute t1] with FIFO")
    result.isRight shouldBe true
    result.value match {
      case ScheduleExpr(_, strategy) =>
        strategy.name shouldBe "FIFO"
      case _ => fail("Expected ScheduleExpr")
    }
  }

  it should "parse schedule with PRIORITY strategy" in {
    val result = Parser.parse("schedule parallel [execute t1] with PRIORITY")
    result.isRight shouldBe true
    result.value match {
      case ScheduleExpr(_, strategy) =>
        strategy.name shouldBe "PRIORITY"
      case _ => fail("Expected ScheduleExpr")
    }
  }

  // ============================================
  // WORKFLOW DEFINITIONS
  // ============================================

  it should "parse workflow definition" in {
    val result = Parser.parse("define workflow myWorkflow = sequence [execute t1]")
    result.isRight shouldBe true
    result.value match {
      case WorkflowDef(name, body) =>
        name shouldBe "myWorkflow"
        body match {
          case TaskSequence(_) => succeed
          case _ => fail("Expected TaskSequence as body")
        }
      case _ => fail("Expected WorkflowDef")
    }
  }

  // ============================================
  // ERROR HANDLING WITH POSITIONS
  // ============================================

  it should "report error position for unexpected character" in {
    val result = Parser.parse("1 + @")
    result.isLeft shouldBe true
    result.left.value match {
      case ParseError(reason, position) =>
        reason should include("Unexpected character")
        position shouldBe Some(4)
      case _ => fail("Expected ParseError")
    }
  }

  it should "report error position for missing closing paren" in {
    val result = Parser.parse("(1 + 2")
    result.isLeft shouldBe true
    result.left.value match {
      case ParseError(reason, _) =>
        reason should include(")")
      case _ => fail("Expected ParseError")
    }
  }

  it should "report error position for missing 'in' in let" in {
    val result = Parser.parse("let x = 5 x")
    result.isLeft shouldBe true
    result.left.value match {
      case ParseError(reason, _) =>
        reason should include("in")
      case _ => fail("Expected ParseError")
    }
  }

  it should "report error position for missing 'then' in if" in {
    val result = Parser.parse("if true 1 else 2")
    result.isLeft shouldBe true
    result.left.value match {
      case ParseError(reason, _) =>
        reason should include("then")
      case _ => fail("Expected ParseError")
    }
  }

  it should "report error position for missing 'else' in if" in {
    val result = Parser.parse("if true then 1")
    result.isLeft shouldBe true
    result.left.value match {
      case ParseError(reason, _) =>
        reason should include("else")
      case _ => fail("Expected ParseError")
    }
  }

  it should "report error for invalid task definition" in {
    val result = Parser.parse("define task = 42")
    result.isLeft shouldBe true
    result.left.value match {
      case ParseError(reason, _) =>
        reason should include("task name")
      case _ => fail("Expected ParseError")
    }
  }

  it should "report error for unexpected tokens after expression" in {
    val result = Parser.parse("1 + 2 3")
    result.isLeft shouldBe true
    result.left.value match {
      case ParseError(reason, _) =>
        reason should include("Unexpected tokens")
      case _ => fail("Expected ParseError")
    }
  }

  // ============================================
  // WHITESPACE HANDLING
  // ============================================

  it should "handle extra whitespace" in {
    Parser.parse("  1   +   2  ") shouldBe Right(BinOp(Add, NumLit(1), NumLit(2)))
  }

  it should "handle newlines as whitespace" in {
    Parser.parse("1\n+\n2") shouldBe Right(BinOp(Add, NumLit(1), NumLit(2)))
  }

  it should "handle tabs as whitespace" in {
    Parser.parse("1\t+\t2") shouldBe Right(BinOp(Add, NumLit(1), NumLit(2)))
  }
}