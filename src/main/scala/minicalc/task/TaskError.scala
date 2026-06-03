package minicalc.task

import scala.concurrent.duration.FiniteDuration

/**
 * Base trait for all task-related errors.
 *
 * Provides a common interface for error handling across
 * the task execution system.
 */
sealed trait TaskError {

  /**
   * Human-readable error message.
   *
   * @return Description of the error
   */
  def message: String
}

/**
 * Error during task execution.
 *
 * Wraps exceptions or other failures that occur while
 * running a task's action.
 *
 * @param message Description of the error
 * @param cause   Optional underlying exception
 */
case class ExecutionError(message: String, cause: Option[Throwable]) extends TaskError

/**
 * Task exceeded its configured timeout.
 *
 * @param duration The timeout duration that was exceeded
 */
case class TimeoutError(duration: FiniteDuration) extends TaskError {
  def message: String = s"Task timed out after $duration"
}

/**
 * Task has dependencies that don't exist.
 *
 * @param missingDeps Set of TaskIds that couldn't be found
 */
case class DependencyError(missingDeps: Set[TaskId]) extends TaskError {
  def message: String = s"Missing dependencies: ${missingDeps.map(_.value).mkString(", ")}"
}

/**
 * Circular dependency detected in task graph.
 *
 * @param cycle List of TaskIds forming the cycle
 */
case class CircularDependencyError(cycle: List[TaskId]) extends TaskError {
  def message: String = s"Circular dependency detected: ${cycle.map(_.value).mkString(" -> ")}"
}

/**
 * Task configuration is invalid.
 *
 * @param reason Description of why the task is invalid
 */
case class InvalidTaskError(reason: String) extends TaskError {
  def message: String = s"Invalid task: $reason"
}

/**
 * Error accessing a required resource.
 *
 * @param resource Name or identifier of the resource
 * @param reason   Description of the access failure
 */
case class ResourceError(resource: String, reason: String) extends TaskError {
  def message: String = s"Resource error ($resource): $reason"
}