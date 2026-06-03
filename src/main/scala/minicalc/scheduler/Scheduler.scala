package minicalc.scheduler

import cats.effect.IO

import minicalc.task.{Task, TaskId}

/**
 * Trait defining the interface for task schedulers.
 *
 * A scheduler is responsible for:
 * - Ordering tasks based on their dependencies
 * - Detecting cycles in the dependency graph
 * - Applying scheduling strategies (FIFO, Priority, etc.)
 *
 * Implementations should ensure that dependencies are always
 * executed before the tasks that depend on them.
 */
trait Scheduler {

  /**
   * Schedules a list of tasks according to dependencies and strategy.
   *
   * First resolves dependencies to ensure a valid execution order,
   * then applies the scheduling strategy to determine final ordering.
   *
   * @param tasks    List of tasks to schedule
   * @param strategy Strategy for ordering tasks after dependency resolution
   * @tparam A       The result type of the tasks
   * @return IO containing either a SchedulingError or the ordered task list
   */
  def schedule[A](
      tasks: List[Task[A]],
      strategy: SchedulingStrategy
  ): IO[Either[SchedulingError, List[Task[A]]]]

  /**
   * Resolves task dependencies and returns tasks in dependency order.
   *
   * Uses topological sorting to ensure that for any task T, all of T's
   * dependencies appear before T in the result list.
   *
   * @param tasks List of tasks with dependency information
   * @tparam A    The result type of the tasks
   * @return Either a SchedulingError or the dependency-ordered task list
   */
  def resolveDependencies[A](
      tasks: List[Task[A]]
  ): Either[SchedulingError, List[Task[A]]]

  /**
   * Detects cycles in the task dependency graph.
   *
   * Uses depth-first search to find back edges, which indicate cycles.
   *
   * @param tasks List of tasks to check
   * @tparam A    The result type of the tasks
   * @return Some(cycle) if a cycle exists, None otherwise
   */
  def detectCycles[A](tasks: List[Task[A]]): Option[List[TaskId]]
}