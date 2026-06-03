package minicalc.execution

import scala.concurrent.duration.FiniteDuration

/**
 * Configuration for task execution behavior.
 *
 * Controls how tasks are executed by the FiberExecutionEngine, including
 * concurrency limits, timeouts, and failure handling strategies.
 *
 * @param maxConcurrency Maximum number of tasks that can run in parallel.
 *                       Must be positive.
 * @param defaultTimeout Optional default timeout applied to tasks without
 *                       their own timeout. None means no timeout.
 * @param failFast       If true, stop execution immediately when any task fails.
 *                       If false, continue executing remaining tasks.
 */
case class ExecutionConfig(
    maxConcurrency: Int,
    defaultTimeout: Option[FiniteDuration],
    failFast: Boolean
) {
  // Validation
  require(maxConcurrency > 0, "maxConcurrency must be positive")
  require(defaultTimeout.forall(_.toMillis > 0), "defaultTimeout must be positive")
}

/**
 * Companion object with factory methods for ExecutionConfig.
 */
object ExecutionConfig {

  /**
   * Creates a default execution configuration.
   *
   * Default settings:
   * - maxConcurrency: 10 (up to 10 tasks can run in parallel)
   * - defaultTimeout: None (no timeout)
   * - failFast: false (continue executing even if a task fails)
   *
   * @return A sensible default ExecutionConfig
   */
  def default: ExecutionConfig = {
    ExecutionConfig(
      maxConcurrency = 10,
      defaultTimeout = None,
      failFast = false
    )
  }
}