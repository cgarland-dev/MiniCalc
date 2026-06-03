package minicalc.monitoring

import cats.effect.IO
import scala.concurrent.duration.FiniteDuration

import minicalc.task.{TaskId, TaskResult}

// ============================================
// PUBLIC STATISTICS
// ============================================

/**
 * Public statistics interface for displaying execution metrics.
 *
 * This is the user-facing view of execution statistics, with computed
 * values like average duration. Returned by TaskMonitor.getStatistics.
 *
 * @param tasksStarted     Total number of tasks that have been started
 * @param tasksCompleted   Total number of tasks that completed successfully
 * @param tasksFailed      Total number of tasks that failed
 * @param averageDuration  Average duration of completed and failed tasks
 *                         (excludes skipped tasks)
 * @param currentlyRunning Set of task IDs that are currently executing
 */
case class ExecutionStatistics(
    tasksStarted: Int,
    tasksCompleted: Int,
    tasksFailed: Int,
    averageDuration: FiniteDuration,
    currentlyRunning: Set[TaskId]
)

// ============================================
// TASK MONITOR TRAIT
// ============================================

/**
 * Trait for monitoring task execution lifecycle and statistics.
 *
 * Implementations track task starts, completions, failures, and timing
 * information. Designed for concurrent access - implementations should
 * be thread-safe.
 *
 * Typical usage:
 * {{{
 * for {
 *   _      <- monitor.onTaskStart(taskId)
 *   result <- executeTask(task)
 *   _      <- monitor.onTaskComplete(result)
 *   stats  <- monitor.getStatistics
 * } yield stats
 * }}}
 */
trait TaskMonitor {

  /**
   * Records that a task has started execution.
   *
   * Should be called immediately before a task begins executing.
   *
   * @param taskId The ID of the task that is starting
   * @return IO[Unit] representing the recording action
   */
  def onTaskStart(taskId: TaskId): IO[Unit]

  /**
   * Records that a task has completed (successfully, failed, or skipped).
   *
   * Should be called immediately after a task finishes, regardless of outcome.
   *
   * @param result The TaskResult containing the outcome and timing information
   * @return IO[Unit] representing the recording action
   */
  def onTaskComplete(result: TaskResult[_]): IO[Unit]

  /**
   * Retrieves the current execution statistics.
   *
   * Returns a snapshot of the current state. The values may change
   * immediately after reading if tasks are still executing.
   *
   * @return IO containing the current ExecutionStatistics
   */
  def getStatistics: IO[ExecutionStatistics]

  /**
   * Resets all statistics to their initial values.
   *
   * Clears all counters and timing information. Useful for starting
   * a fresh monitoring session.
   *
   * @return IO[Unit] representing the reset action
   */
  def reset: IO[Unit]
}

// ============================================
// INTERNAL STATE
// ============================================

/**
 * Internal state container for task monitoring.
 *
 * This is the internal representation used by monitor implementations.
 * Unlike ExecutionStatistics, it stores raw totals (totalDuration)
 * rather than computed values (averageDuration).
 *
 * Should not be exposed directly to users - use ExecutionStatistics instead.
 *
 * @param tasksStarted     Total number of tasks that have been started
 * @param tasksCompleted   Total number of tasks that completed successfully
 * @param tasksFailed      Total number of tasks that failed
 * @param totalDuration    Sum of all finished task durations (for computing average)
 * @param currentlyRunning Set of task IDs that are currently executing
 */
case class MonitorState(
    tasksStarted: Int,
    tasksCompleted: Int,
    tasksFailed: Int,
    totalDuration: FiniteDuration,
    currentlyRunning: Set[TaskId]
)