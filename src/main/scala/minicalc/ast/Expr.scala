package minicalc.ast

import minicalc.scheduler.SchedulingStrategy

/**
 * Abstract Syntax Tree (AST) definitions for MiniCalc expressions.
 * 
 * Uses sealed traits for exhaustive pattern matching - the compiler
 * will warn if we miss a case when evaluating expressions.
 */

// ============================================================================
// FINAL PROJECT EXTENSION: Retry Configuration
// ============================================================================

/**
 * Retry configuration for tasks.
 * 
 * @param maxAttempts Maximum number of retry attempts
 * @param backoffSeconds Base delay between retries (in seconds)
 * @param exponential If true, use exponential backoff; otherwise linear
 */
case class RetryConfig(
  maxAttempts: Int,
  backoffSeconds: Double,
  exponential: Boolean
)

// ============================================================================
// BASE EXPRESSION TRAIT
// ============================================================================

/** Base trait for all expression types */
sealed trait Expr

// ============================================================================
// MIDTERM EXPRESSIONS (original MiniCalc)
// ============================================================================

/** Numeric literal - e.g., 42, 3.14 */
case class NumLit(value: Double) extends Expr

/** Boolean literal - true or false */
case class BoolLit(value: Boolean) extends Expr

/** Variable reference - looks up the variable name in the environment */
case class Var(name: String) extends Expr

/** Binary operation - applies an operator to two sub-expressions */
case class BinOp(op: BinaryOp, left: Expr, right: Expr) extends Expr

/** Let binding - binds a variable for use in the body expression
  * Example: let x = 5 in x + 3
  */
case class Let(name: String, value: Expr, body: Expr) extends Expr

/** Conditional expression - evaluates thenExpr or elseExpr based on condition
  * Example: if x > 5 then 100 else 200
  */
case class If(cond: Expr, thenExpr: Expr, elseExpr: Expr) extends Expr

// ============================================================================
// FINAL PROJECT EXTENSIONS (task orchestration)
// ============================================================================

/**
 * Task definition - creates a task with configuration.
 * Example: define task fetchData = httpGet("api.example.com") with priority High
 * 
 * @param name Task name (identifier)
 * @param action The expression to execute when the task runs
 * @param priority Optional priority expression (evaluates to "High", "Normal", or "Low")
 * @param dependencies List of task names this task depends on
 * @param timeout Optional timeout expression (evaluates to seconds)
 * @param retryConfig Optional retry configuration
 */
case class TaskDef(
  name: String,
  action: Expr,
  priority: Option[Expr],
  dependencies: List[String],
  timeout: Option[Expr],
  retryConfig: Option[RetryConfig]
) extends Expr

/**
 * Task execution - executes a previously defined task by name.
 * Example: execute fetchData
 * 
 * @param taskName Name of the task to execute
 */
case class TaskExec(taskName: String) extends Expr

/**
 * Sequential task execution - runs tasks one after another.
 * Example: sequence [task1, task2, task3]
 * 
 * @param tasks List of task expressions to run sequentially
 */
case class TaskSequence(tasks: List[Expr]) extends Expr

/**
 * Parallel task execution - runs tasks concurrently.
 * Example: parallel [task1, task2, task3]
 * 
 * @param tasks List of task expressions to run in parallel
 */
case class TaskParallel(tasks: List[Expr]) extends Expr

/**
 * Schedule and execute tasks with a specific strategy.
 * Example: schedule [task1, task2, task3] with FIFO
 * 
 * @param tasks Expression that evaluates to a list of tasks
 * @param strategy Scheduling strategy (FIFO or Priority)
 */
case class ScheduleExpr(tasks: Expr, strategy: SchedulingStrategy) extends Expr

/**
  * Workflow definition
  * Example: define workflow workflowName [expr1,expr2]
  *
  * @param name
  * @param body
  */
case class WorkflowDef(name: String, body: Expr) extends Expr