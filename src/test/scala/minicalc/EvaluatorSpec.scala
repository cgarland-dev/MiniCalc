package minicalc.eval

import cats.effect.IO
import cats.effect.unsafe.implicits.global
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

import minicalc.ast._

/**
 * Comprehensive tests for the MiniCalc evaluator.
 */
class EvaluatorSpec extends AnyFlatSpec with Matchers {

  // Helper to create a fresh environment for each test
  def freshEnv: Environment = Environment.create.unsafeRunSync()

  // ============================================
  // LITERAL EVALUATION
  // ============================================

  "Evaluator.eval" should "evaluate number literals" in {
    val env = freshEnv
    Evaluator.eval(NumLit(42), env) shouldBe Right(NumValue(42))
    Evaluator.eval(NumLit(3.14), env) shouldBe Right(NumValue(3.14))
    Evaluator.eval(NumLit(0), env) shouldBe Right(NumValue(0))
  }

  it should "evaluate boolean literals" in {
    val env = freshEnv
    Evaluator.eval(BoolLit(true), env) shouldBe Right(BoolValue(true))
    Evaluator.eval(BoolLit(false), env) shouldBe Right(BoolValue(false))
  }

  // ============================================
  // VARIABLE EVALUATION
  // ============================================

  it should "evaluate defined variables" in {
    val env = freshEnv
    val envWithX = env.add("x", NumValue(42))
    Evaluator.eval(Var("x"), envWithX) shouldBe Right(NumValue(42))
  }

  it should "return error for undefined variables" in {
    val env = freshEnv
    val result = Evaluator.eval(Var("undefined"), env)
    result.isLeft shouldBe true
    result.left.toOption.get shouldBe a[UndefinedVariable]
  }

  // ============================================
  // ARITHMETIC OPERATIONS
  // ============================================

  it should "evaluate addition" in {
    val env = freshEnv
    Evaluator.eval(BinOp(Add, NumLit(1), NumLit(2)), env) shouldBe Right(NumValue(3))
    Evaluator.eval(BinOp(Add, NumLit(1.5), NumLit(2.5)), env) shouldBe Right(NumValue(4.0))
  }

  it should "evaluate subtraction" in {
    val env = freshEnv
    Evaluator.eval(BinOp(Sub, NumLit(5), NumLit(3)), env) shouldBe Right(NumValue(2))
    Evaluator.eval(BinOp(Sub, NumLit(1), NumLit(5)), env) shouldBe Right(NumValue(-4))
  }

  it should "evaluate multiplication" in {
    val env = freshEnv
    Evaluator.eval(BinOp(Mul, NumLit(4), NumLit(3)), env) shouldBe Right(NumValue(12))
    Evaluator.eval(BinOp(Mul, NumLit(2.5), NumLit(4)), env) shouldBe Right(NumValue(10.0))
  }

  it should "evaluate division" in {
    val env = freshEnv
    Evaluator.eval(BinOp(Div, NumLit(10), NumLit(2)), env) shouldBe Right(NumValue(5))
    Evaluator.eval(BinOp(Div, NumLit(7), NumLit(2)), env) shouldBe Right(NumValue(3.5))
  }

  it should "return error for division by zero" in {
    val env = freshEnv
    val result = Evaluator.eval(BinOp(Div, NumLit(10), NumLit(0)), env)
    result.isLeft shouldBe true
    result.left.toOption.get shouldBe a[DivisionByZero]
  }

  it should "evaluate nested arithmetic expressions" in {
    val env = freshEnv
    // (1 + 2) * 3 = 9
    val expr = BinOp(Mul, BinOp(Add, NumLit(1), NumLit(2)), NumLit(3))
    Evaluator.eval(expr, env) shouldBe Right(NumValue(9))
  }

  // ============================================
  // COMPARISON OPERATIONS
  // ============================================

  it should "evaluate less than" in {
    val env = freshEnv
    Evaluator.eval(BinOp(Lt, NumLit(1), NumLit(2)), env) shouldBe Right(BoolValue(true))
    Evaluator.eval(BinOp(Lt, NumLit(2), NumLit(1)), env) shouldBe Right(BoolValue(false))
    Evaluator.eval(BinOp(Lt, NumLit(1), NumLit(1)), env) shouldBe Right(BoolValue(false))
  }

  it should "evaluate greater than" in {
    val env = freshEnv
    Evaluator.eval(BinOp(Gt, NumLit(2), NumLit(1)), env) shouldBe Right(BoolValue(true))
    Evaluator.eval(BinOp(Gt, NumLit(1), NumLit(2)), env) shouldBe Right(BoolValue(false))
  }

  it should "evaluate less than or equal" in {
    val env = freshEnv
    Evaluator.eval(BinOp(Lte, NumLit(1), NumLit(2)), env) shouldBe Right(BoolValue(true))
    Evaluator.eval(BinOp(Lte, NumLit(1), NumLit(1)), env) shouldBe Right(BoolValue(true))
    Evaluator.eval(BinOp(Lte, NumLit(2), NumLit(1)), env) shouldBe Right(BoolValue(false))
  }

  it should "evaluate greater than or equal" in {
    val env = freshEnv
    Evaluator.eval(BinOp(Gte, NumLit(2), NumLit(1)), env) shouldBe Right(BoolValue(true))
    Evaluator.eval(BinOp(Gte, NumLit(1), NumLit(1)), env) shouldBe Right(BoolValue(true))
    Evaluator.eval(BinOp(Gte, NumLit(1), NumLit(2)), env) shouldBe Right(BoolValue(false))
  }

  it should "evaluate equality" in {
    val env = freshEnv
    Evaluator.eval(BinOp(Eq, NumLit(1), NumLit(1)), env) shouldBe Right(BoolValue(true))
    Evaluator.eval(BinOp(Eq, NumLit(1), NumLit(2)), env) shouldBe Right(BoolValue(false))
  }

  it should "evaluate inequality" in {
    val env = freshEnv
    Evaluator.eval(BinOp(Neq, NumLit(1), NumLit(2)), env) shouldBe Right(BoolValue(true))
    Evaluator.eval(BinOp(Neq, NumLit(1), NumLit(1)), env) shouldBe Right(BoolValue(false))
  }

  // ============================================
  // LOGICAL OPERATIONS
  // ============================================

  it should "evaluate logical AND" in {
    val env = freshEnv
    Evaluator.eval(BinOp(And, BoolLit(true), BoolLit(true)), env) shouldBe Right(BoolValue(true))
    Evaluator.eval(BinOp(And, BoolLit(true), BoolLit(false)), env) shouldBe Right(BoolValue(false))
    Evaluator.eval(BinOp(And, BoolLit(false), BoolLit(true)), env) shouldBe Right(BoolValue(false))
    Evaluator.eval(BinOp(And, BoolLit(false), BoolLit(false)), env) shouldBe Right(BoolValue(false))
  }

  it should "evaluate logical OR" in {
    val env = freshEnv
    Evaluator.eval(BinOp(Or, BoolLit(true), BoolLit(true)), env) shouldBe Right(BoolValue(true))
    Evaluator.eval(BinOp(Or, BoolLit(true), BoolLit(false)), env) shouldBe Right(BoolValue(true))
    Evaluator.eval(BinOp(Or, BoolLit(false), BoolLit(true)), env) shouldBe Right(BoolValue(true))
    Evaluator.eval(BinOp(Or, BoolLit(false), BoolLit(false)), env) shouldBe Right(BoolValue(false))
  }

  // ============================================
  // TYPE MISMATCH ERRORS
  // ============================================

  it should "return error for arithmetic on booleans" in {
    val env = freshEnv
    val result = Evaluator.eval(BinOp(Add, BoolLit(true), NumLit(1)), env)
    result.isLeft shouldBe true
    result.left.toOption.get shouldBe a[TypeMismatch]
  }

  it should "return error for logical operations on numbers" in {
    val env = freshEnv
    val result = Evaluator.eval(BinOp(And, NumLit(1), NumLit(2)), env)
    result.isLeft shouldBe true
    result.left.toOption.get shouldBe a[TypeMismatch]
  }

  // ============================================
  // LET BINDINGS
  // ============================================

  it should "evaluate let bindings" in {
    val env = freshEnv
    val expr = Let("x", NumLit(5), Var("x"))
    Evaluator.eval(expr, env) shouldBe Right(NumValue(5))
  }

  it should "evaluate let bindings with body expression" in {
    val env = freshEnv
    val expr = Let("x", NumLit(5), BinOp(Add, Var("x"), NumLit(1)))
    Evaluator.eval(expr, env) shouldBe Right(NumValue(6))
  }

  it should "evaluate nested let bindings" in {
    val env = freshEnv
    val expr = Let("x", NumLit(1), Let("y", NumLit(2), BinOp(Add, Var("x"), Var("y"))))
    Evaluator.eval(expr, env) shouldBe Right(NumValue(3))
  }

  it should "support variable shadowing in nested let" in {
    val env = freshEnv
    val expr = Let("x", NumLit(1), Let("x", NumLit(2), Var("x")))
    Evaluator.eval(expr, env) shouldBe Right(NumValue(2))
  }

  // ============================================
  // CONDITIONALS
  // ============================================

  it should "evaluate if-then branch when condition is true" in {
    val env = freshEnv
    val expr = If(BoolLit(true), NumLit(1), NumLit(2))
    Evaluator.eval(expr, env) shouldBe Right(NumValue(1))
  }

  it should "evaluate if-else branch when condition is false" in {
    val env = freshEnv
    val expr = If(BoolLit(false), NumLit(1), NumLit(2))
    Evaluator.eval(expr, env) shouldBe Right(NumValue(2))
  }

  it should "evaluate if with comparison condition" in {
    val env = freshEnv
    val expr = If(BinOp(Gt, NumLit(5), NumLit(3)), NumLit(1), NumLit(2))
    Evaluator.eval(expr, env) shouldBe Right(NumValue(1))
  }

  it should "return error for non-boolean condition" in {
    val env = freshEnv
    val expr = If(NumLit(42), NumLit(1), NumLit(2))
    val result = Evaluator.eval(expr, env)
    result.isLeft shouldBe true
    result.left.toOption.get shouldBe a[TypeMismatch]
  }

  // ============================================
  // TASK EXPRESSIONS IN PURE EVAL
  // ============================================

  it should "return error for TaskDef in pure eval" in {
    val env = freshEnv
    val expr = TaskDef("test", NumLit(42), None, List.empty, None, None)
    val result = Evaluator.eval(expr, env)
    result.isLeft shouldBe true
    result.left.toOption.get shouldBe a[UnsupportedInPureEval]
  }

  it should "return error for TaskExec in pure eval" in {
    val env = freshEnv
    val result = Evaluator.eval(TaskExec("test"), env)
    result.isLeft shouldBe true
    result.left.toOption.get shouldBe a[UnsupportedInPureEval]
  }

  // ============================================
  // IO-BASED EVALUATION
  // ============================================

  "Evaluator.evalIO" should "evaluate simple expressions through IO" in {
    val env = freshEnv
    val result = Evaluator.evalIO(BinOp(Add, NumLit(1), NumLit(2)), env).unsafeRunSync()
    result.isRight shouldBe true
    result.toOption.get._1 shouldBe NumValue(3)
  }

  it should "evaluate let bindings through IO" in {
    val env = freshEnv
    val expr = Let("x", NumLit(5), BinOp(Mul, Var("x"), NumLit(2)))
    val result = Evaluator.evalIO(expr, env).unsafeRunSync()
    result.isRight shouldBe true
    result.toOption.get._1 shouldBe NumValue(10)
  }

  it should "evaluate conditionals through IO" in {
    val env = freshEnv
    val expr = If(BinOp(Lt, NumLit(1), NumLit(2)), NumLit(100), NumLit(200))
    val result = Evaluator.evalIO(expr, env).unsafeRunSync()
    result.isRight shouldBe true
    result.toOption.get._1 shouldBe NumValue(100)
  }

  it should "evaluate task definitions and add to environment" in {
    val env = freshEnv
    val taskDef = TaskDef("myTask", NumLit(42), None, List.empty, None, None)
    val result = Evaluator.evalIO(taskDef, env).unsafeRunSync()
    result.isRight shouldBe true
    val (value, newEnv) = result.toOption.get
    value shouldBe a[TaskValue]
    newEnv.getTask("myTask").isDefined shouldBe true
  }

  it should "return TaskNotFound for executing non-existent task" in {
    val env = freshEnv
    val result = Evaluator.evalIO(TaskExec("nonExistent"), env).unsafeRunSync()
    result.isLeft shouldBe true
    result.left.toOption.get shouldBe a[TaskNotFound]
  }

  it should "evaluate workflow definitions and add to environment" in {
    val env = freshEnv
    val workflowDef = WorkflowDef("myWorkflow", TaskSequence(List.empty))
    val result = Evaluator.evalIO(workflowDef, env).unsafeRunSync()
    result.isRight shouldBe true
    val (value, newEnv) = result.toOption.get
    value shouldBe a[WorkflowValue]
    newEnv.getWorkflow("myWorkflow").isDefined shouldBe true
  }

  // ============================================
  // COMPLEX EXPRESSIONS
  // ============================================

  it should "evaluate complex nested expressions" in {
    val env = freshEnv
    // let x = 5 in let y = x + 3 in if y > 7 then y * 2 else y
    val expr = Let("x", NumLit(5),
      Let("y", BinOp(Add, Var("x"), NumLit(3)),
        If(BinOp(Gt, Var("y"), NumLit(7)),
          BinOp(Mul, Var("y"), NumLit(2)),
          Var("y")
        )
      )
    )
    Evaluator.eval(expr, env) shouldBe Right(NumValue(16)) // y = 8, 8 > 7, so 8 * 2 = 16
  }
}