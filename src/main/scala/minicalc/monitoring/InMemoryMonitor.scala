package minicalc.monitoring

import cats.effect.{IO, Ref}
import scala.concurrent.duration.{DurationInt, FiniteDuration}

import minicalc.task.{Failure, Skipped, Success, TaskId, TaskResult}

/**
 * Thread-safe in-memory implementation of TaskMonitor.
 *
 * Uses a Cats Effect Ref for atomic state updates, ensuring thread safety
 * when multiple tasks report their status concurrently. All state mutations
 * are performed atomically via `ref.update`.
 *
 * The monitor tracks:
 * - Number of tasks started, completed, and failed
 * - Total execution duration (for computing averages)
 * - Set of currently running task IDs
 *
 * @param ref Atomic reference containing the current MonitorState
 */
class InMemoryMonitor(ref: Ref[IO, MonitorState]) extends TaskMonitor {

  // ============================================
  // TASK LIFECYCLE EVENTS
  // ============================================

  /**
   * Records the start of a task.
   *
   * Atomically increments the started counter and adds the task ID
   * to the currently running set.
   *
   * @param taskId The ID of the task that started
   * @return IO[Unit] representing the state update
   */
  override def onTaskStart(taskId: TaskId): IO[Unit] = {
    ref.update { state =>
      state.copy(
        tasksStarted = state.tasksStarted + 1,
        currentlyRunning = state.currentlyRunning + taskId
      )
    }
  }

  /**
   * Records the completion of a task.
   *
   * Updates the appropriate counter based on the result type:
   * - Success: increments completed, adds duration
   * - Failure: increments failed, adds duration
   * - Skipped: only removes from currently running (doesn't affect averages)
   *
   * In all cases, removes the task from the currently running set.
   *
   * @param result The TaskResult indicating how the task completed
   * @return IO[Unit] representing the state update
   */
  override def onTaskComplete(result: TaskResult[?]): IO[Unit] = {
    result match {
      case Success(taskId, _, duration) =>
        ref.update { state =>
          state.copy(
            tasksCompleted = state.tasksCompleted + 1,
            totalDuration = state.totalDuration + duration,
            currentlyRunning = state.currentlyRunning - taskId
          )
        }

      case Failure(taskId, _, duration) =>
        ref.update { state =>
          state.copy(
            tasksFailed = state.tasksFailed + 1,
            totalDuration = state.totalDuration + duration,
            currentlyRunning = state.currentlyRunning - taskId
          )
        }

      case Skipped(taskId, _) =>
        ref.update { state =>
          state.copy(
            currentlyRunning = state.currentlyRunning - taskId
          )
        }
    }
  }

  // ============================================
  // STATISTICS RETRIEVAL
  // ============================================

  /**
   * Retrieves the current execution statistics.
   *
   * Computes the average duration from the total duration and the number
   * of completed + failed tasks. Skipped tasks are excluded from the
   * average calculation since they have no meaningful duration.
   *
   * @return IO containing the current ExecutionStatistics
   */
  override def getStatistics: IO[ExecutionStatistics] = {
    ref.get.map { state =>
      // Calculate average duration, avoiding division by zero
      val finishedTaskCount = state.tasksCompleted + state.tasksFailed
      val averageDuration = finishedTaskCount match {
        case 0     => 0.seconds
        case total => state.totalDuration / total
      }

      ExecutionStatistics(
        tasksStarted = state.tasksStarted,
        tasksCompleted = state.tasksCompleted,
        tasksFailed = state.tasksFailed,
        averageDuration = averageDuration,
        currentlyRunning = state.currentlyRunning
      )
    }
  }

  // ============================================
  // STATE MANAGEMENT
  // ============================================

  /**
   * Resets all statistics to their initial values.
   *
   * Clears all counters, durations, and the currently running set.
   * Useful for starting a fresh monitoring session.
   *
   * @return IO[Unit] representing the state reset
   */
  override def reset: IO[Unit] = {
    ref.set(
      MonitorState(
        tasksStarted = 0,
        tasksCompleted = 0,
        tasksFailed = 0,
        totalDuration = 0.seconds,
        currentlyRunning = Set.empty
      )
    )
  }
}

/**
 * Companion object with factory methods for InMemoryMonitor.
 */
object InMemoryMonitor {

  /**
   * Creates a new InMemoryMonitor with initial empty state.
   *
   * This is an IO action because creating a Ref requires IO. The Ref
   * provides the thread-safe atomic state container needed for concurrent
   * task monitoring.
   *
   * @return IO that produces a new InMemoryMonitor instance
   */
  def create: IO[InMemoryMonitor] = {
    Ref
      .of[IO, MonitorState](
        MonitorState(
          tasksStarted = 0,
          tasksCompleted = 0,
          tasksFailed = 0,
          totalDuration = 0.seconds,
          currentlyRunning = Set.empty
        )
      )
      .map(ref => new InMemoryMonitor(ref))
  }
}