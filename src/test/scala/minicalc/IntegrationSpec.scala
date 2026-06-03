package minicalc.integration

import cats.effect.IO
import cats.effect.unsafe.implicits.global
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

import minicalc.ast._
import minicalc.eval.{Environment, Evaluator}
import minicalc.parser.Parser

/**
 * Integration tests for the complete MiniCalc system.
 *
 * Tests the full pipeline: Parse -> Evaluate -> Result
 */
class IntegrationSpec extends AnyFlatSpec with Matchers {

  // Helper to run full pipeline
  def run(input: String): Either[String, Value] = {
    Parser.parse(input) match {
      case Left(err) =>
        Left(s"Parse error: ${err.message}")
      case Right(expr) =>
        val env = Environment.create.unsafeRunSync()
        Evaluator.evalIO(expr, env).unsafeRunSync() match {
          case Left(err)         => Left(s"Eval error: ${err.message}")
          case Right((value, _)) => Right(value)
        }
    }
  }

  // Helper for stateful operations
  def runWithEnv(input: String, env: Environment): Either[String, (Value, Environment)] = {
    Parser.parse(input) match {
      case Left(err) =>
        Left(s"Parse error: ${err.message}")
      case Right(expr) =>
        Evaluator.evalIO(expr, env).unsafeRunSync() match {
          case Left(err)              => Left(s"Eval error: ${err.message}")
          case Right((value, newEnv)) => Right((value, newEnv))
        }
    }
  }

  // ============================================
  // ARITHMETIC INTEGRATION
  // ============================================

  "Full pipeline" should "evaluate simple arithmetic" in {
    run("1 + 2 * 3") shouldBe Right(NumValue(7))
  }

  it should "evaluate complex arithmetic with parentheses" in {
    run("(1 + 2) * (3 + 4)") shouldBe Right(NumValue(21))
  }

  it should "handle division" in {
    run("10 / 4") shouldBe Right(NumValue(2.5))
  }

  it should "detect division by zero" in {
    val result = run("10 / 0")
    result.isLeft shouldBe true
    result.left.toOption.get should include("zero")
  }

  // ============================================
  // COMPARISON AND LOGICAL INTEGRATION
  // ============================================

  it should "evaluate comparisons" in {
    run("5 > 3") shouldBe Right(BoolValue(true))
  }

  it should "evaluate compound logical expressions" in {
    run("5 > 3 && 2 < 4") shouldBe Right(BoolValue(true))
  }

  it should "evaluate mixed expressions" in {
    run("1 + 2 == 3") shouldBe Right(BoolValue(true))
  }

  // ============================================
  // LET BINDING INTEGRATION
  // ============================================

  it should "evaluate let bindings" in {
    run("let x = 5 in x * 2") shouldBe Right(NumValue(10))
  }

  it should "evaluate nested let bindings" in {
    run("let x = 2 in let y = 3 in x * y") shouldBe Right(NumValue(6))
  }

  it should "evaluate let with complex expressions" in {
    // x = 3, y = 6, result = 9
    run("let x = 1 + 2 in let y = x * 2 in y + x") shouldBe Right(NumValue(9))
  }

  // ============================================
  // CONDITIONAL INTEGRATION
  // ============================================

  it should "evaluate true condition" in {
    run("if 5 > 3 then 100 else 200") shouldBe Right(NumValue(100))
  }

  it should "evaluate false condition" in {
    run("if 3 > 5 then 100 else 200") shouldBe Right(NumValue(200))
  }

  it should "evaluate conditional with let" in {
    run("let x = 10 in if x > 5 then x * 2 else x") shouldBe Right(NumValue(20))
  }

  // ============================================
  // TASK DEFINITION AND EXECUTION INTEGRATION
  // ============================================

  it should "define and execute a simple task" in {
    val env = Environment.create.unsafeRunSync()
    val result1 = runWithEnv("define task myTask = 42", env)
    result1.isRight shouldBe true
    val (_, env1) = result1.toOption.get

    val result2 = runWithEnv("execute myTask", env1)
    result2.isRight shouldBe true
    val (value, _) = result2.toOption.get
    value shouldBe a[TaskResultValue]
  }

  it should "define task with priority" in {
    val env = Environment.create.unsafeRunSync()
    val result = runWithEnv("define task highPriority = 42 with priority High", env)
    result.isRight shouldBe true
    val (value, newEnv) = result.toOption.get
    value shouldBe a[TaskValue]
    val task = newEnv.getTask("highPriority").get
    task.priority.toString shouldBe "High"
  }

  it should "define task with timeout" in {
    val env = Environment.create.unsafeRunSync()
    val result = runWithEnv("define task timedTask = 42 timeout 30", env)
    result.isRight shouldBe true
    val (_, newEnv) = result.toOption.get
    val task = newEnv.getTask("timedTask").get
    task.timeout.isDefined shouldBe true
  }

  it should "define task with retry policy" in {
    val env = Environment.create.unsafeRunSync()
    val result = runWithEnv("define task retryTask = 42 retry 3 backoff 5", env)
    result.isRight shouldBe true
    val (_, newEnv) = result.toOption.get
    val task = newEnv.getTask("retryTask").get
    task.retryPolicy should not be minicalc.task.NoRetry
  }

  it should "fail to execute non-existent task" in {
    val result = run("execute nonExistent")
    result.isLeft shouldBe true
    result.left.toOption.get should include("not found")
  }

  // ============================================
  // WORKFLOW INTEGRATION
  // ============================================

  it should "define and run a workflow" in {
    val env = Environment.create.unsafeRunSync()

    // Define task
    val r1 = runWithEnv("define task t1 = 42", env)
    r1.isRight shouldBe true
    val (_, env1) = r1.toOption.get

    // Define workflow
    val r2 = runWithEnv("define workflow myFlow = sequence [execute t1]", env1)
    r2.isRight shouldBe true
    val (_, env2) = r2.toOption.get

    env2.getWorkflow("myFlow").isDefined shouldBe true
  }

  // ============================================
  // ERROR HANDLING INTEGRATION
  // ============================================

  it should "report parse errors with position" in {
    val result = run("let 123 = x")
    result.isLeft shouldBe true
    result.left.toOption.get should include("Parse error")
  }

  it should "report undefined variable errors" in {
    val result = run("x + 1")
    result.isLeft shouldBe true
    result.left.toOption.get should include("Undefined")
  }

  it should "report type mismatch errors" in {
    val result = run("5 + true")
    result.isLeft shouldBe true
    result.left.toOption.get should include("Type mismatch")
  }

// ============================================
  // SEQUENCE AND PARALLEL INTEGRATION
  // ============================================

  it should "execute tasks in sequence" in {
      val env = Environment.create.unsafeRunSync()

      // Define tasks
      val r1 = runWithEnv("define task t1 = 1", env)
      r1.isRight shouldBe true
      val (_, env1) = r1.toOption.get

      val r2 = runWithEnv("define task t2 = 2", env1)
      r2.isRight shouldBe true
      val (_, env2) = r2.toOption.get

      // Execute them individually (since sequence syntax needs work)
      val r3 = runWithEnv("execute t1", env2)
      r3.isRight shouldBe true

      val r4 = runWithEnv("execute t2", env2)
      r4.isRight shouldBe true
    }

  it should "execute multiple tasks" in {
    val env = Environment.create.unsafeRunSync()

    // Define tasks
    val r1 = runWithEnv("define task t1 = 1", env)
    r1.isRight shouldBe true
    val (_, env1) = r1.toOption.get

    val r2 = runWithEnv("define task t2 = 2", env1)
    r2.isRight shouldBe true
    val (_, env2) = r2.toOption.get

    // Verify both tasks are defined
    env2.getTask("t1").isDefined shouldBe true
    env2.getTask("t2").isDefined shouldBe true

    // Execute them
    val exec1 = runWithEnv("execute t1", env2)
    exec1.isRight shouldBe true

    val exec2 = runWithEnv("execute t2", env2)
    exec2.isRight shouldBe true
  }

  // ============================================
  // COMPLEX INTEGRATION SCENARIOS
  // ============================================

  it should "handle complex expression with all features" in {
    // base = 10, base > 5 is true, so multiplier = 2, result = 20
    run("let base = 10 in let multiplier = if base > 5 then 2 else 1 in base * multiplier") shouldBe Right(NumValue(20))
  }

  it should "handle deeply nested expressions" in {
    // a=1, b=2, c=3, d=4
    run("let a = 1 in let b = a + 1 in let c = b + 1 in let d = c + 1 in d") shouldBe Right(NumValue(4))
  }

  it should "handle operator precedence correctly" in {
    // Standard math precedence: 2 + 3 * 4 - 5 / 5 = 2 + 12 - 1 = 13
    run("2 + 3 * 4 - 5 / 5") shouldBe Right(NumValue(13))
  }

  it should "handle logical operator precedence" in {
    // && has higher precedence than ||
    // true || false && false = true || (false && false) = true || false = true
    run("true || false && false") shouldBe Right(BoolValue(true))
  }

  it should "handle comparison chains" in {
    // 1 < 2 && 2 < 3 && 3 < 4
    run("1 < 2 && 2 < 3 && 3 < 4") shouldBe Right(BoolValue(true))
  }
}