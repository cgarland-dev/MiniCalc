package minicalc.task

import cats.effect.IO
import scala.concurrent.duration.FiniteDuration

/**
 * Represents an executable task with dependencies and configuration.
 *
 * A task encapsulates:
 * - An IO action to execute
 * - Identity information (id, name)
 * - Scheduling configuration (priority, dependencies)
 * - Execution configuration (timeout, retry policy)
 * - Optional metadata for custom attributes
 *
 * Tasks are immutable - use TaskBuilder to construct them.
 *
 * @param id           Unique identifier for this task
 * @param name         Human-readable name
 * @param priority     Execution priority (High, Normal, Low)
 * @param dependencies Set of task IDs that must complete before this task
 * @param action       The IO action to execute
 * @param timeout      Optional maximum execution time
 * @param retryPolicy  Strategy for handling failures
 * @param metadata     Custom key-value pairs for extensibility
 * @tparam A           The result type of the task action
 */
case class Task[A](
    id: TaskId,
    name: String,
    priority: Priority,
    dependencies: Set[TaskId],
    action: IO[A],
    timeout: Option[FiniteDuration],
    retryPolicy: RetryPolicy,
    metadata: Map[String, String]
) {

  /**
   * Checks if this task has any dependencies.
   *
   * @return true if the task depends on other tasks
   */
  def hasDeps: Boolean = dependencies.nonEmpty

  /**
   * Checks if this task has a timeout configured.
   *
   * @return true if a timeout is set
   */
  def hasTimeout: Boolean = timeout.isDefined
}