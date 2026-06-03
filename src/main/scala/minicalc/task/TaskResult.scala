package minicalc.task

import scala.concurrent.duration.FiniteDuration

/**
 * Result of executing a task.
 *
 * Represents the three possible outcomes of task execution:
 * - Success: task completed and produced a value
 * - Failure: task encountered an error
 * - Skipped: task was not executed (e.g., dependency failed)
 *
 * @tparam A The result type of the task
 */
sealed trait TaskResult[A] {

  /**
   * The ID of the task that produced this result.
   */
  def taskId: TaskId

  /**
   * Human-readable description of the result.
   */
  def message: String

  /**
   * Extracts the value if this is a Success.
   *
   * @return Some(value) for Success, None for Failure/Skipped
   */
  def toOption: Option[A] = {
    this match {
      case Success(_, value, _) => Some(value)
      case _                    => None
    }
  }

  /**
   * Checks if this result represents successful completion.
   *
   * @return true for Success, false otherwise
   */
  def isSuccess: Boolean = this.isInstanceOf[Success[A]]

  /**
   * Checks if this result represents a failure.
   *
   * @return true for Failure, false otherwise
   */
  def isFailure: Boolean = this.isInstanceOf[Failure[A]]

  /**
   * Checks if this task was skipped.
   *
   * @return true for Skipped, false otherwise
   */
  def isSkipped: Boolean = this.isInstanceOf[Skipped[A]]
}

/**
 * Successful task completion.
 *
 * @param taskId   The task that completed
 * @param value    The result value produced
 * @param duration How long the task took to execute
 * @tparam A       The result type
 */
case class Success[A](
    taskId: TaskId,
    value: A,
    duration: FiniteDuration
) extends TaskResult[A] {

  def message: String = {
    s"Task ${taskId.value} succeeded with $value in $duration"
  }
}

/**
 * Failed task execution.
 *
 * @param taskId   The task that failed
 * @param error    The error that occurred
 * @param duration How long before the task failed
 * @tparam A       The result type (never produced)
 */
case class Failure[A](
    taskId: TaskId,
    error: TaskError,
    duration: FiniteDuration
) extends TaskResult[A] {

  def message: String = {
    s"Task ${taskId.value} failed: ${error.message} (after $duration)"
  }
}

/**
 * Task was skipped without execution.
 *
 * Typically occurs when a dependency failed or the task
 * was cancelled.
 *
 * @param taskId The task that was skipped
 * @param reason Why the task was skipped
 * @tparam A     The result type (never produced)
 */
case class Skipped[A](
    taskId: TaskId,
    reason: String
) extends TaskResult[A] {

  def message: String = {
    s"Task ${taskId.value} skipped: $reason"
  }
}