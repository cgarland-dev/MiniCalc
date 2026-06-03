package minicalc.task

import cats.effect.IO
import scala.concurrent.duration.FiniteDuration

/**
 * Factory object for creating TaskBuilder instances.
 *
 * Provides two entry points:
 * - `task`: for simple synchronous actions (wraps in IO.delay)
 * - `taskIO`: for actions already wrapped in IO
 */
object TaskBuilder {

  /**
   * Creates a task builder for a synchronous action.
   *
   * The action is wrapped in IO.delay for lazy evaluation.
   *
   * @param name   Human-readable name for the task
   * @param action The action to execute (by-name, evaluated lazily)
   * @tparam A     The result type of the action
   * @return A TaskBuilderPartial for fluent configuration
   */
  def task[A](name: String)(action: => A): TaskBuilderPartial[A] = {
    TaskBuilderPartial(name, IO.delay(action))
  }

  /**
   * Creates a task builder for an IO action.
   *
   * Use this when you already have an IO-wrapped action.
   *
   * @param name   Human-readable name for the task
   * @param action The IO action to execute
   * @tparam A     The result type of the action
   * @return A TaskBuilderPartial for fluent configuration
   */
  def taskIO[A](name: String)(action: IO[A]): TaskBuilderPartial[A] = {
    TaskBuilderPartial(name, action)
  }
}

/**
 * Fluent builder for constructing Task instances.
 *
 * Provides a chainable API for configuring task properties before
 * building the final immutable Task object.
 *
 * Example usage:
 * {{{
 * val task = TaskBuilder.task("myTask") { computeResult() }
 *   .withPriority(High)
 *   .withTimeout(30.seconds)
 *   .withRetry(3, 1.second)
 *   .dependsOn(otherTaskId)
 *   .build
 * }}}
 *
 * @param name   Human-readable name for the task
 * @param action The IO action to execute
 * @tparam A     The result type of the action
 */
class TaskBuilderPartial[A](
    private val name: String,
    private val action: IO[A]
) {
  // Configuration fields with sensible defaults
  private var id: TaskId = TaskId.generate()
  private var priority: Priority = Normal
  private var dependencies: Set[TaskId] = Set.empty
  private var timeout: Option[FiniteDuration] = None
  private var retryPolicy: RetryPolicy = NoRetry
  private var metadata: Map[String, String] = Map.empty

  /**
   * Sets a specific task ID.
   *
   * @param taskId The ID to use
   * @return this builder for chaining
   */
  def withId(taskId: TaskId): TaskBuilderPartial[A] = {
    this.id = taskId
    this
  }

  /**
   * Sets the task priority.
   *
   * @param p The priority level
   * @return this builder for chaining
   */
  def withPriority(p: Priority): TaskBuilderPartial[A] = {
    this.priority = p
    this
  }

  /**
   * Declares dependencies on other tasks.
   *
   * This task will not execute until all dependencies have completed.
   *
   * @param taskIds The IDs of tasks this task depends on
   * @return this builder for chaining
   */
  def dependsOn(taskIds: TaskId*): TaskBuilderPartial[A] = {
    this.dependencies = taskIds.toSet
    this
  }

  /**
   * Sets an execution timeout.
   *
   * The task will be cancelled if it exceeds this duration.
   *
   * @param duration Maximum execution time
   * @return this builder for chaining
   */
  def withTimeout(duration: FiniteDuration): TaskBuilderPartial[A] = {
    this.timeout = Some(duration)
    this
  }

  /**
   * Configures linear retry policy.
   *
   * Failed tasks will be retried with constant delay between attempts.
   *
   * @param maxAttempts Maximum number of attempts
   * @param backoff     Delay between attempts
   * @return this builder for chaining
   */
  def withRetry(maxAttempts: Int, backoff: FiniteDuration): TaskBuilderPartial[A] = {
    this.retryPolicy = LinearRetry(maxAttempts, backoff)
    this
  }

  /**
   * Configures exponential backoff retry policy.
   *
   * Failed tasks will be retried with exponentially increasing delays.
   *
   * @param maxAttempts Maximum number of attempts
   * @param baseDelay   Initial delay (doubles with each attempt)
   * @return this builder for chaining
   */
  def withExponentialRetry(maxAttempts: Int, baseDelay: FiniteDuration): TaskBuilderPartial[A] = {
    this.retryPolicy = ExponentialRetry(maxAttempts, baseDelay)
    this
  }

  /**
   * Adds a metadata entry.
   *
   * Metadata can be used for custom attributes, logging, or tracing.
   *
   * @param key   Metadata key
   * @param value Metadata value
   * @return this builder for chaining
   */
  def addMetadata(key: String, value: String): TaskBuilderPartial[A] = {
    this.metadata = this.metadata + (key -> value)
    this
  }

  /**
   * Builds the final immutable Task object.
   *
   * @return The configured Task instance
   */
  def build: Task[A] = {
    Task(
      id = this.id,
      name = this.name,
      action = this.action,
      priority = this.priority,
      dependencies = this.dependencies,
      timeout = this.timeout,
      retryPolicy = this.retryPolicy,
      metadata = this.metadata
    )
  }
}

/**
 * Companion object for TaskBuilderPartial with factory method.
 */
object TaskBuilderPartial {

  /**
   * Creates a new TaskBuilderPartial.
   *
   * @param name   Task name
   * @param action Task action
   * @tparam A     Result type
   * @return New builder instance
   */
  def apply[A](name: String, action: IO[A]): TaskBuilderPartial[A] = {
    new TaskBuilderPartial(name, action)
  }
}