package minicalc.execution

import scala.concurrent.duration.FiniteDuration

import minicalc.task.TaskResult

/**
 * Report containing the results and statistics of a task execution run.
 *
 * Provides comprehensive information about what happened during execution,
 * including counts, timing, and individual task results. Useful for
 * monitoring, logging, and making decisions based on execution outcomes.
 *
 * @param totalTasks    Total number of tasks that were submitted for execution
 * @param successful    Number of tasks that completed successfully
 * @param failed        Number of tasks that failed (threw exceptions or timed out)
 * @param skipped       Number of tasks that were skipped (e.g., due to failed dependencies)
 * @param totalDuration Wall-clock time for the entire execution run
 * @param results       List of individual TaskResult objects for each task
 */
case class ExecutionReport(
    totalTasks: Int,
    successful: Int,
    failed: Int,
    skipped: Int,
    totalDuration: FiniteDuration,
    results: List[TaskResult[_]]
) {

  // ============================================
  // RATE CALCULATIONS
  // ============================================

  /**
   * Calculates the success rate as a decimal (0.0 to 1.0).
   *
   * @return Proportion of tasks that succeeded, or 0.0 if no tasks were executed
   */
  def successRate: Double = {
    if (totalTasks == 0) 0.0 else successful.toDouble / totalTasks
  }

  /**
   * Calculates the failure rate as a decimal (0.0 to 1.0).
   *
   * @return Proportion of tasks that failed, or 0.0 if no tasks were executed
   */
  def failureRate: Double = {
    if (totalTasks == 0) 0.0 else failed.toDouble / totalTasks
  }

  /**
   * Calculates the skip rate as a decimal (0.0 to 1.0).
   *
   * @return Proportion of tasks that were skipped, or 0.0 if no tasks were executed
   */
  def skipRate: Double = {
    if (totalTasks == 0) 0.0 else skipped.toDouble / totalTasks
  }

  // ============================================
  // STATUS CHECKS
  // ============================================

  /**
   * Checks if all tasks completed successfully.
   *
   * @return true if every task succeeded, false otherwise
   */
  def allSucceeded: Boolean = {
    successful == totalTasks
  }

  /**
   * Checks if any task failed.
   *
   * @return true if at least one task failed, false otherwise
   */
  def anyFailed: Boolean = {
    failed > 0
  }

  /**
   * Validates that the counts are consistent.
   *
   * The sum of successful, failed, and skipped should equal totalTasks.
   * Useful for debugging and ensuring report integrity.
   *
   * @return true if the counts add up correctly, false otherwise
   */
  def validate: Boolean = {
    totalTasks == successful + failed + skipped
  }

  // ============================================
  // DISPLAY
  // ============================================

  /**
   * Generates a human-readable summary of the execution.
   *
   * @return A formatted string describing the execution results
   */
  def summary: String = {
    val successPercent = (successRate * 100).toInt
    s"Executed $totalTasks tasks in $totalDuration. " +
      s"Success rate: $successPercent%. " +
      s"Failed: $failed. Skipped: $skipped."
  }
}