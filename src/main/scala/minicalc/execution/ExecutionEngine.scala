package minicalc.execution

import cats.effect.IO

import minicalc.task.{Task, TaskResult}

/**
 * Trait defining the interface for task execution engines.
 *
 * Provides multiple execution strategies for running tasks:
 * - Simple execution with configuration
 * - Concurrent execution with parallelism limits
 * - Dependency-aware execution with topological ordering
 *
 * Implementations should handle task scheduling, concurrency control,
 * error handling, and result collection.
 */
trait ExecutionEngine {

  /**
   * Executes a list of tasks according to the provided configuration.
   *
   * Tasks are executed respecting the configuration's concurrency limits,
   * timeout settings, and failure handling strategy.
   *
   * @param tasks  List of tasks to execute
   * @param config Execution configuration controlling behavior
   * @tparam A     The result type of the tasks
   * @return IO containing an ExecutionReport with results and statistics
   */
  def execute[A](tasks: List[Task[A]], config: ExecutionConfig): IO[ExecutionReport]

  /**
   * Executes tasks concurrently with a specified parallelism limit.
   *
   * Unlike `execute`, this method returns the raw task results without
   * wrapping them in an ExecutionReport. Useful for simple concurrent
   * execution without needing detailed statistics.
   *
   * @param tasks          List of tasks to execute
   * @param maxConcurrency Maximum number of tasks to run in parallel
   * @tparam A             The result type of the tasks
   * @return IO containing a list of TaskResults in completion order
   */
  def executeConcurrently[A](tasks: List[Task[A]], maxConcurrency: Int): IO[List[TaskResult[A]]]

  /**
   * Executes tasks respecting their dependency relationships.
   *
   * Tasks are scheduled in topological order based on their declared
   * dependencies. A task will not start until all its dependencies
   * have completed successfully.
   *
   * @param tasks List of tasks with dependency information
   * @tparam A    The result type of the tasks
   * @return IO containing an ExecutionReport with results and statistics
   */
  def executeWithDependencies[A](tasks: List[Task[A]]): IO[ExecutionReport]
}