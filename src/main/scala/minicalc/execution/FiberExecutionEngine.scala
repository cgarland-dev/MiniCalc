package minicalc.execution

import cats.effect.IO
import cats.effect.implicits._
import cats.syntax.monadError._
import scala.concurrent.duration._

import minicalc.scheduler._
import minicalc.task._

/**
 * Fiber-based execution engine using Cats Effect for concurrent task execution.
 *
 * Implements the ExecutionEngine trait using lightweight fibers for parallelism.
 * Supports:
 * - Concurrent execution with configurable parallelism limits
 * - Timeout handling per task
 * - Retry policies (linear and exponential backoff)
 * - Dependency-aware execution via scheduler integration
 * - Continue-on-error mode for batch execution
 *
 * @param scheduler Scheduler used for dependency resolution and task ordering
 */
class FiberExecutionEngine(scheduler: Scheduler) extends ExecutionEngine {

  // ============================================
  // SINGLE TASK EXECUTION
  // ============================================

  /**
   * Executes a single task with timeout and retry handling.
   *
   * Handles the complete lifecycle of task execution:
   * 1. Records start time
   * 2. Applies timeout if configured
   * 3. Applies retry policy if configured
   * 4. Runs the task action
   * 5. Captures any errors with context
   * 6. Records end time and duration
   * 7. Returns appropriate TaskResult (Success or Failure)
   *
   * @param task The task to execute
   * @tparam A   The result type of the task
   * @return IO containing the TaskResult
   */
  private def executeTask[A](task: Task[A]): IO[TaskResult[A]] = {
    for {
      startTime <- IO.realTime

      result <- {
        // Apply timeout if configured on the task
        val actionWithTimeout = task.timeout match {
          case Some(duration) =>
            task.action.timeout(duration).adaptError { case _: java.util.concurrent.TimeoutException =>
              TaskTimeoutException(task.id, task.name, duration)
            }
          case None =>
            task.action
        }

        // Apply retry policy if configured
        val actionWithRetry = task.retryPolicy match {
          case NoRetry =>
            actionWithTimeout

          case LinearRetry(maxAttempts, delay) =>
            retryWithPolicy(actionWithTimeout, maxAttempts, attempt => delay, task)

          case ExponentialRetry(maxAttempts, baseDelay) =>
            retryWithPolicy(actionWithTimeout, maxAttempts, attempt => baseDelay * math.pow(2, attempt - 1).toLong, task)
        }

        // Run the action and capture any errors as Either
        actionWithRetry.attempt
      }

      endTime <- IO.realTime
      duration = endTime - startTime
    } yield {
      // Convert Either result to TaskResult with rich error context
      result match {
        case Right(value) =>
          Success(task.id, value, duration)

        case Left(error: TaskTimeoutException) =>
          Failure(
            task.id,
            TimeoutError(error.duration),
            duration
          )

        case Left(error: TaskRetryExhaustedException) =>
          Failure(
            task.id,
            ExecutionError(
              s"Task '${task.name}' failed after ${error.attempts} attempts: ${error.lastError}",
              error.cause
            ),
            duration
          )

        case Left(error) =>
          Failure(
            task.id,
            ExecutionError(
              s"Task '${task.name}' failed: ${Option(error.getMessage).getOrElse("Unknown error")}",
              Some(error)
            ),
            duration
          )
      }
    }
  }

  /**
   * Retries an action according to a retry policy.
   *
   * @param action      The action to retry
   * @param maxAttempts Maximum number of attempts
   * @param delayFn     Function to calculate delay for each attempt
   * @param task        The task (for error context)
   * @tparam A          Result type
   * @return IO with the result or final error
   */
  private def retryWithPolicy[A](
      action: IO[A],
      maxAttempts: Int,
      delayFn: Int => FiniteDuration,
      task: Task[_]
  ): IO[A] = {
    def loop(attempt: Int, lastError: Option[Throwable]): IO[A] = {
      if (attempt > maxAttempts) {
        IO.raiseError(TaskRetryExhaustedException(
          task.id,
          task.name,
          maxAttempts,
          lastError.map(_.getMessage).getOrElse("Unknown error"),
          lastError
        ))
      } else {
        action.handleErrorWith { error =>
          if (attempt < maxAttempts) {
            // Wait and retry
            IO.sleep(delayFn(attempt)) >> loop(attempt + 1, Some(error))
          } else {
            // Max attempts reached
            IO.raiseError(TaskRetryExhaustedException(
              task.id,
              task.name,
              maxAttempts,
              Option(error.getMessage).getOrElse("Unknown error"),
              Some(error)
            ))
          }
        }
      }
    }

    loop(1, None)
  }

  // ============================================
  // EXECUTION ENGINE IMPLEMENTATION
  // ============================================

  /**
   * Executes a list of tasks according to the provided configuration.
   *
   * Runs tasks concurrently up to the maxConcurrency limit specified
   * in the configuration. Collects all results and timing information
   * into an ExecutionReport.
   *
   * @param tasks  List of tasks to execute
   * @param config Execution configuration controlling concurrency
   * @tparam A     The result type of the tasks
   * @return IO containing an ExecutionReport with all results and statistics
   */
  override def execute[A](tasks: List[Task[A]], config: ExecutionConfig): IO[ExecutionReport] = {
    for {
      startTime <- IO.realTime
      results   <- executeConcurrently(tasks, config.maxConcurrency)
      endTime   <- IO.realTime
    } yield {
      val duration = endTime - startTime

      // Count results by status
      val successCount = results.count(_.isSuccess)
      val failureCount = results.count(_.isFailure)
      val skippedCount = results.count(_.isSkipped)

      ExecutionReport(
        totalTasks = tasks.size,
        successful = successCount,
        failed = failureCount,
        skipped = skippedCount,
        totalDuration = duration,
        results = results
      )
    }
  }

  /**
   * Executes tasks concurrently with a specified parallelism limit.
   *
   * Uses Cats Effect's `parTraverseN` to run tasks in parallel while
   * respecting the concurrency limit. This prevents resource exhaustion
   * when executing many tasks.
   *
   * @param tasks          List of tasks to execute
   * @param maxConcurrency Maximum number of tasks to run simultaneously
   * @tparam A             The result type of the tasks
   * @return IO containing a list of TaskResults
   */
  override def executeConcurrently[A](
      tasks: List[Task[A]],
      maxConcurrency: Int
  ): IO[List[TaskResult[A]]] = {
    tasks.parTraverseN(maxConcurrency)(task => executeTask(task))
  }

  /**
   * Executes tasks respecting their dependency relationships.
   *
   * Uses the scheduler to determine the correct execution order based
   * on task dependencies. Tasks are sorted topologically so that
   * dependencies are executed before the tasks that depend on them.
   *
   * If scheduling fails (e.g., due to a cycle in dependencies), returns
   * a report with all tasks marked as failed.
   *
   * @param tasks List of tasks with dependency information
   * @tparam A    The result type of the tasks
   * @return IO containing an ExecutionReport
   */
  override def executeWithDependencies[A](tasks: List[Task[A]]): IO[ExecutionReport] = {
    for {
      scheduledResult <- scheduler.schedule(tasks, FIFOStrategy)

      report <- scheduledResult match {
        case Right(orderedTasks) =>
          // Successfully scheduled - execute in dependency order
          execute(orderedTasks, ExecutionConfig.default)

        case Left(CircularDependency(cycle)) =>
          // Circular dependency - mark all tasks as failed
          IO.pure(ExecutionReport(
            totalTasks = tasks.size,
            successful = 0,
            failed = tasks.size,
            skipped = 0,
            totalDuration = 0.seconds,
            results = tasks.map(t => Failure[A](
              t.id,
              CircularDependencyError(cycle),
              0.seconds
            ))
          ))

        case Left(UnresolvableDependencies(unresolved)) =>
          // Missing dependencies - mark affected tasks as failed
          IO.pure(ExecutionReport(
            totalTasks = tasks.size,
            successful = 0,
            failed = tasks.size,
            skipped = 0,
            totalDuration = 0.seconds,
            results = tasks.map(t => Failure[A](
              t.id,
              DependencyError(unresolved),
              0.seconds
            ))
          ))
      }
    } yield report
  }
}

// ============================================
// CUSTOM EXCEPTIONS FOR BETTER ERROR CONTEXT
// ============================================

/**
 * Exception thrown when a task times out.
 *
 * @param taskId   The ID of the task that timed out
 * @param taskName The name of the task
 * @param duration The timeout duration that was exceeded
 */
case class TaskTimeoutException(
    taskId: TaskId,
    taskName: String,
    duration: FiniteDuration
) extends Exception(s"Task '$taskName' (${taskId.value}) timed out after $duration")

/**
 * Exception thrown when all retry attempts are exhausted.
 *
 * @param taskId    The ID of the task that failed
 * @param taskName  The name of the task
 * @param attempts  Number of attempts made
 * @param lastError The error message from the last attempt
 * @param cause     The underlying exception from the last attempt
 */
case class TaskRetryExhaustedException(
    taskId: TaskId,
    taskName: String,
    attempts: Int,
    lastError: String,
    cause: Option[Throwable]
) extends Exception(s"Task '$taskName' failed after $attempts attempts: $lastError", cause.orNull)