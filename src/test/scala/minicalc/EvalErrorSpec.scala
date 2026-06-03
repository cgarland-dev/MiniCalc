package minicalc.eval

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

/**
 * Tests for EvalError types and their messages.
 *
 * Ensures error messages are informative and consistent.
 */
class EvalErrorSpec extends AnyFlatSpec with Matchers {

  // ============================================
  // ARITHMETIC ERRORS
  // ============================================

  "DivisionByZero" should "produce informative message" in {
    val error = DivisionByZero("10.0 / 0.0")
    error.message should include("divide by zero")
    error.message should include("10.0 / 0.0")
  }

  // ============================================
  // VARIABLE ERRORS
  // ============================================

  "UndefinedVariable" should "include variable name in message" in {
    val error = UndefinedVariable("myVar")
    error.message should include("myVar")
    error.message should include("Undefined")
  }

  // ============================================
  // TYPE ERRORS
  // ============================================

  "TypeMismatch" should "include expected and actual types" in {
    val error = TypeMismatch("Number", "Boolean", "true")
    error.message should include("Number")
    error.message should include("Boolean")
    error.message should include("true")
  }

  // ============================================
  // PARSE ERRORS
  // ============================================

  "ParseError" should "include position when available" in {
    val error = ParseError("Unexpected token", Some(42))
    error.message should include("42")
    error.message should include("Unexpected token")
  }

  it should "work without position" in {
    val error = ParseError("Syntax error", None)
    error.message should include("Syntax error")
    error.message should not include("position")
  }

  it should "provide context with messageWithContext" in {
    val error = ParseError("Expected expression", Some(4))
    val input = "let 123 = x"
    val contextMsg = error.messageWithContext(input)

    contextMsg should include("position 4")
    contextMsg should include("let 123 = x")
    contextMsg should include("^")
  }

  it should "handle multi-line input in messageWithContext" in {
    val input = "line1\nlet 123 = x"
    val error = ParseError("Expected variable", Some(10)) // Position in second line
    val contextMsg = error.messageWithContext(input)

    contextMsg should include("line 2")
  }

  // ============================================
  // TASK ERRORS
  // ============================================

  "TaskNotFound" should "include task name" in {
    val error = TaskNotFound("myTask")
    error.message should include("myTask")
    error.message should include("not found")
  }

  "WorkflowNotFound" should "include workflow name" in {
    val error = WorkflowNotFound("myWorkflow")
    error.message should include("myWorkflow")
    error.message should include("not found")
  }

  "TaskExecutionFailed" should "include task name and reason" in {
    val error = TaskExecutionFailed("myTask", "Connection timeout")
    error.message should include("myTask")
    error.message should include("Connection timeout")
    error.message should include("failed")
  }

  "UnresolvedDependencies" should "list all missing dependencies" in {
    val error = UnresolvedDependencies("myTask", List("dep1", "dep2", "dep3"))
    error.message should include("myTask")
    error.message should include("dep1")
    error.message should include("dep2")
    error.message should include("dep3")
  }

  "CircularDependencyDetected" should "show the cycle" in {
    val error = CircularDependencyDetected(List("A", "B", "C", "A"))
    error.message should include("Circular")
    error.message should include("A")
    error.message should include("B")
    error.message should include("C")
    error.message should include("->")
  }

  // ============================================
  // SCHEDULING ERRORS
  // ============================================

  "SchedulingFailed" should "include reason" in {
    val error = SchedulingFailed("Too many tasks")
    error.message should include("Scheduling failed")
    error.message should include("Too many tasks")
  }

  // ============================================
  // INTERNAL ERRORS
  // ============================================

  "UnsupportedInPureEval" should "indicate IO is needed" in {
    val error = UnsupportedInPureEval("TaskDef")
    error.message should include("TaskDef")
    error.message should include("evalIO")
  }

  "InternalError" should "include reason" in {
    val error = InternalError("Unexpected state")
    error.message should include("Internal error")
    error.message should include("Unexpected state")
  }
}