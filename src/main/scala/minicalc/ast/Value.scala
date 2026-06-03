package minicalc.ast

import minicalc.task.{Task, TaskResult}

/**
 * Runtime values that expressions can evaluate to.
 *
 * The accessor methods (asNumber, asBoolean, asTask, etc.) provide type-safe extraction
 * using Option - returning Some(value) if the type matches, None otherwise.
 */

/** Base trait for all values */
sealed trait Value {

  /** Try to extract this value as a number */
  def asNumber: Option[Double]

  /** Try to extract this value as a boolean */
  def asBoolean: Option[Boolean]

  /** Try to extract this value as a task */
  def asTask: Option[Task[Value]]

  /** Try to extract this value as a task result */
  def asTaskResult: Option[TaskResult[Value]]

  /** Try to extract this value as a list of tasks */
  def asTaskList: Option[List[Task[Value]]]

  /** Try to extract this value as a workflow */
  def asWorkflow: Option[(String, Expr)]

  /** Convert this value to a string for display */
  def show: String

  /**
   * Returns the type name of this value for error messages.
   *
   * Used in TypeMismatch errors to provide clear feedback like:
   * "Type mismatch: expected Number, got Boolean"
   */
  def typeName: String = this match {
    case _: NumValue             => "Number"
    case _: BoolValue            => "Boolean"
    case _: TaskValue            => "Task"
    case _: TaskResultValue      => "TaskResult"
    case _: TaskListValue        => "TaskList"
    case _: WorkflowValue        => "Workflow"
    case _: ExecutionReportValue => "ExecutionReport"
  }
}

// ============================================================================
// MIDTERM VALUES (original MiniCalc)
// ============================================================================

/**
 * Numeric value - the result of evaluating arithmetic expressions.
 *
 * @param n The numeric value
 */
case class NumValue(n: Double) extends Value {
  override def asNumber: Option[Double] = Some(n)
  override def asBoolean: Option[Boolean] = None
  override def asTask: Option[Task[Value]] = None
  override def asTaskResult: Option[TaskResult[Value]] = None
  override def asTaskList: Option[List[Task[Value]]] = None
  override def asWorkflow: Option[(String, Expr)] = None
  override def show: String = s"$n"
}

/**
 * Boolean value - the result of evaluating comparisons and logical operations.
 *
 * @param b The boolean value
 */
case class BoolValue(b: Boolean) extends Value {
  override def asNumber: Option[Double] = None
  override def asBoolean: Option[Boolean] = Some(b)
  override def asTask: Option[Task[Value]] = None
  override def asTaskResult: Option[TaskResult[Value]] = None
  override def asTaskList: Option[List[Task[Value]]] = None
  override def asWorkflow: Option[(String, Expr)] = None
  override def show: String = s"$b"
}

// ============================================================================
// FINAL PROJECT VALUE EXTENSIONS (task orchestration)
// ============================================================================

/**
 * Task value - represents a defined task that can be executed.
 *
 * @param t The task definition
 */
case class TaskValue(t: Task[Value]) extends Value {
  override def asNumber: Option[Double] = None
  override def asBoolean: Option[Boolean] = None
  override def asTask: Option[Task[Value]] = Some(t)
  override def asTaskResult: Option[TaskResult[Value]] = None
  override def asTaskList: Option[List[Task[Value]]] = None
  override def asWorkflow: Option[(String, Expr)] = None
  override def show: String = s"Task(${t.id.value})"
}

/**
 * Task result value - represents the result of executing a task.
 *
 * @param r The task execution result (Success, Failure, or Skipped)
 */
case class TaskResultValue(r: TaskResult[Value]) extends Value {
  override def asNumber: Option[Double] = None
  override def asBoolean: Option[Boolean] = None
  override def asTask: Option[Task[Value]] = None
  override def asTaskResult: Option[TaskResult[Value]] = Some(r)
  override def asTaskList: Option[List[Task[Value]]] = None
  override def asWorkflow: Option[(String, Expr)] = None
  override def show: String = r.message
}

/**
 * Task list value - represents a list of tasks.
 *
 * @param l List of task definitions
 */
case class TaskListValue(l: List[Task[Value]]) extends Value {
  override def asNumber: Option[Double] = None
  override def asBoolean: Option[Boolean] = None
  override def asTask: Option[Task[Value]] = None
  override def asTaskResult: Option[TaskResult[Value]] = None
  override def asTaskList: Option[List[Task[Value]]] = Some(l)
  override def asWorkflow: Option[(String, Expr)] = None
  override def show: String = s"TaskList(${l.size} tasks)"
}

/**
 * Workflow value - represents a workflow name and body.
 *
 * @param name Name of workflow
 * @param body Workflow body (executable expression)
 */
case class WorkflowValue(name: String, body: Expr) extends Value {
  override def asNumber: Option[Double] = None
  override def asBoolean: Option[Boolean] = None
  override def asTask: Option[Task[Value]] = None
  override def asTaskResult: Option[TaskResult[Value]] = None
  override def asTaskList: Option[List[Task[Value]]] = None
  override def asWorkflow: Option[(String, Expr)] = Some((name, body))
  override def show: String = s"Workflow($name)"
}

/**
 * Execution report value - represents the results of executing multiple tasks.
 *
 * @param totalTasks Total number of tasks executed
 * @param successful Number of successful tasks
 * @param failed Number of failed tasks
 * @param results List of individual task results
 */
case class ExecutionReportValue(
    totalTasks: Int,
    successful: Int,
    failed: Int,
    results: List[TaskResult[Value]]
) extends Value {
  override def asNumber: Option[Double] = None
  override def asBoolean: Option[Boolean] = None
  override def asTask: Option[Task[Value]] = None
  override def asTaskResult: Option[TaskResult[Value]] = None
  override def asTaskList: Option[List[Task[Value]]] = None
  override def asWorkflow: Option[(String, Expr)] = None
  
  override def show: String = {
    val header = s"Executed $totalTasks tasks: $successful succeeded, $failed failed"
    val details = results.map(r => s"  ${r.message}").mkString("\n")
    s"$header\n$details"
  }
}