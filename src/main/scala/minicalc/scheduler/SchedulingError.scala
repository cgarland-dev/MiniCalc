package minicalc.scheduler

import minicalc.task.TaskId

/**
 * Base trait for errors that can occur during task scheduling.
 *
 * Scheduling can fail due to invalid dependency graphs (cycles)
 * or missing task definitions (unresolvable dependencies).
 */
sealed trait SchedulingError {

  /**
   * Human-readable error message.
   *
   * @return Description of the scheduling error
   */
  def message: String
}

/**
 * Error indicating a circular dependency was detected in the task graph.
 *
 * A circular dependency occurs when task A depends on task B, which
 * depends on task C, which depends on task A (or any similar cycle).
 * Such graphs cannot be scheduled because there's no valid execution order.
 *
 * @param cycle List of TaskIds forming the cycle (e.g., [A, B, C, A])
 */
case class CircularDependency(cycle: List[TaskId]) extends SchedulingError {
  def message: String = {
    s"Circular dependency detected: ${cycle.map(_.value).mkString(" -> ")}"
  }
}

/**
 * Error indicating some task dependencies could not be resolved.
 *
 * This occurs when a task declares a dependency on another task
 * that doesn't exist in the task list being scheduled.
 *
 * @param tasks Set of TaskIds that have unresolvable dependencies
 */
case class UnresolvableDependencies(tasks: Set[TaskId]) extends SchedulingError {
  def message: String = {
    s"Unresolvable dependencies for tasks: ${tasks.map(_.value).mkString(", ")}"
  }
}