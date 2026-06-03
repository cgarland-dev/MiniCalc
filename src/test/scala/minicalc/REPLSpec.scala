package minicalc.repl

import cats.effect.IO
import cats.effect.unsafe.implicits.global
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

import minicalc.eval.Environment

/**
 * Tests for the REPL input processing.
 */
class REPLSpec extends AnyFlatSpec with Matchers {

  // Helper to process input and get the output string
  def process(input: String, env: Environment): (Environment, String) = {
    REPL.processInputIO(input, env).unsafeRunSync()
  }

  def freshEnv: Environment = Environment.create.unsafeRunSync()

  // ============================================
  // BASIC COMMANDS
  // ============================================

  "REPL" should "handle empty input" in {
    val env = freshEnv
    val (_, output) = process("", env)
    output shouldBe ""
  }

  it should "handle :help command" in {
    val env = freshEnv
    val (_, output) = process(":help", env)
    output should include("Commands")
    output should include(":quit")
    output should include(":env")
  }

  it should "handle :env command with empty environment" in {
    val env = freshEnv
    val (_, output) = process(":env", env)
    output should include("empty")
  }

  it should "handle :tasks command with no tasks" in {
    val env = freshEnv
    val (_, output) = process(":tasks", env)
    output should include("No tasks")
  }

  it should "handle :workflows command with no workflows" in {
    val env = freshEnv
    val (_, output) = process(":workflows", env)
    output should include("No workflows")
  }

  // ============================================
  // EXPRESSION EVALUATION
  // ============================================

  it should "evaluate arithmetic expressions" in {
    val env = freshEnv
    val (_, output) = process("1 + 2 * 3", env)
    output shouldBe "7.0"
  }

  it should "evaluate boolean expressions" in {
    val env = freshEnv
    val (_, output) = process("5 > 3", env)
    output shouldBe "true"
  }

  it should "report parse errors" in {
    val env = freshEnv
    val (_, output) = process("1 + +", env)
    output should include("Parse error")
  }

  it should "report evaluation errors" in {
    val env = freshEnv
    val (_, output) = process("x + 1", env)
    output should include("Error")
    output should include("Undefined")
  }

  // ============================================
  // TASK COMMANDS
  // ============================================

  it should "show tasks after defining one" in {
    val env = freshEnv
    val (env1, _) = process("define task myTask = 42", env)
    val (_, output) = process(":tasks", env1)
    output should include("myTask")
  }

  it should "clear tasks with :clear-tasks" in {
    val env = freshEnv
    val (env1, _) = process("define task myTask = 42", env)
    val (env2, _) = process(":clear-tasks", env1)
    val (_, output) = process(":tasks", env2)
    output should include("No tasks")
  }

  // ============================================
  // STATISTICS COMMANDS
  // ============================================

  it should "show statistics with :stats" in {
    val env = freshEnv
    val (_, output) = process(":stats", env)
    output should include("Statistics")
    output should include("Tasks Started")
    output should include("Tasks Completed")
  }

  it should "reset statistics with :reset-stats" in {
    val env = freshEnv
    val (_, output) = process(":reset-stats", env)
    output should include("reset")
  }

  // ============================================
  // SETTINGS COMMANDS
  // ============================================

  it should "show settings with :settings" in {
    val env = freshEnv
    val (_, output) = process(":settings", env)
    output should include("Settings")
    output should include("continue-on-error")
    output should include("verbose")
    output should include("show-timing")
  }

  it should "change settings with :set" in {
    val env = freshEnv
    val (env1, output1) = process(":set verbose true", env)
    val (_, output2) = process(":settings", env1)
    output1 should include("updated")
    output2 should include("verbose")
    output2 should include("true")
  }

  it should "accept various boolean values for :set" in {
    val env = freshEnv
    val (env1, _) = process(":set verbose on", env)
    val (env2, _) = process(":set verbose off", env1)
    val (env3, _) = process(":set verbose 1", env2)
    val (env4, _) = process(":set verbose 0", env3)

    env1.settings.verbose shouldBe true
    env2.settings.verbose shouldBe false
    env3.settings.verbose shouldBe true
    env4.settings.verbose shouldBe false
  }

  it should "report error for invalid setting name" in {
    val env = freshEnv
    val (_, output) = process(":set invalid-setting true", env)
    output should include("Error")
    output should include("Unknown setting")
  }

  it should "report error for invalid setting value" in {
    val env = freshEnv
    val (_, output) = process(":set verbose maybe", env)
    output should include("Error")
    output should include("Invalid value")
  }

  // ============================================
  // VERBOSE MODE
  // ============================================

  it should "show type information when verbose is enabled" in {
    val env = freshEnv
    val (env1, _) = process(":set verbose true", env)
    val (_, output) = process("42", env1)
    output should include("42")
    output should include("Number")
  }

  // ============================================
  // TIMING MODE
  // ============================================

  it should "show timing when show-timing is enabled" in {
    val env = freshEnv
    val (env1, _) = process(":set show-timing true", env)
    val (_, output) = process("1 + 2", env1)
    output should include("completed in")
  }

  // ============================================
  // CLEAR COMMAND
  // ============================================

  it should "clear environment with :clear" in {
    val env = freshEnv
    val (env1, _) = process("define task myTask = 42", env)
    val (env2, output) = process(":clear", env1)
    val (_, tasksOutput) = process(":tasks", env2)
    output should include("cleared")
    tasksOutput should include("No tasks")
  }

  // ============================================
  // WORKFLOW COMMANDS
  // ============================================

  it should "show workflows after defining one" in {
    val env = freshEnv
    val (env1, _) = process("define task t1 = 42", env)
    val (env2, _) = process("define workflow myFlow = sequence [execute t1]", env1)
    val (_, output) = process(":workflows", env2)
    output should include("myFlow")
  }

  it should "report error for running non-existent workflow" in {
    val env = freshEnv
    val (_, output) = process(":run nonExistent", env)
    output should include("Error")
    output should include("not found")
  }

  // ============================================
  // WHITESPACE HANDLING
  // ============================================

  it should "handle commands with extra whitespace" in {
    val env = freshEnv
    val (_, output) = process("  :help  ", env)
    output should include("Commands")
  }

  it should "handle expressions with extra whitespace" in {
    val env = freshEnv
    val (_, output) = process("  1  +  2  ", env)
    output shouldBe "3.0"
  }
}