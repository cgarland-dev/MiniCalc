package minicalc.scheduler

import cats.effect.IO

import minicalc.task.{Task, TaskId}

/**
 * Scheduler implementation using topological sorting for dependency resolution.
 *
 * Uses two classic graph algorithms:
 * - DFS-based cycle detection (three-color algorithm)
 * - Kahn's algorithm for topological sorting
 *
 * This ensures tasks are executed in an order that respects all
 * dependency constraints, and detects invalid (cyclic) dependency graphs.
 */
class TopologicalScheduler extends Scheduler {

  // ============================================
  // PUBLIC API
  // ============================================

  /**
   * Schedules tasks by resolving dependencies and applying the strategy.
   *
   * First performs dependency resolution to get a valid execution order,
   * then applies the scheduling strategy:
   * - FIFO: keeps dependency order as-is
   * - Priority: sorts by task priority (highest first)
   *
   * @param tasks    List of tasks to schedule
   * @param strategy Strategy for final ordering
   * @tparam A       The result type of the tasks
   * @return IO containing either a SchedulingError or the ordered task list
   */
  override def schedule[A](
      tasks: List[Task[A]],
      strategy: SchedulingStrategy
  ): IO[Either[SchedulingError, List[Task[A]]]] = {
    IO {
      resolveDependencies(tasks).map { orderedTasks =>
        strategy match {
          case FIFOStrategy =>
            // Keep the topological order as-is
            orderedTasks

          case PriorityStrategy =>
            // Sort by priority (highest first)
            orderedTasks.sorted(Ordering.by((t: Task[A]) => t.priority).reverse)
        }
      }
    }
  }

  /**
   * Orders tasks by dependencies using topological sort.
   *
   * First checks for cycles (which make scheduling impossible),
   * then performs topological sort to get a valid execution order.
   *
   * @param tasks List of tasks with dependency information
   * @tparam A    The result type of the tasks
   * @return Either a SchedulingError or the dependency-ordered task list
   */
  override def resolveDependencies[A](
      tasks: List[Task[A]]
  ): Either[SchedulingError, List[Task[A]]] = {
    detectCycles(tasks) match {
      case Some(cycle) =>
        // Cycle found - cannot schedule
        Left(CircularDependency(cycle))

      case None =>
        // No cycles - perform topological sort
        topologicalSort(tasks)
    }
  }

  /**
   * Detects cycles in the task dependency graph using DFS.
   *
   * Uses the three-color algorithm:
   * - White (not in map): unvisited
   * - Gray (true): currently being visited (on the stack)
   * - Black (false): fully processed
   *
   * A cycle exists if we encounter a gray node during DFS.
   *
   * @param tasks List of tasks to check
   * @tparam A    The result type of the tasks
   * @return Some(cycle) listing the TaskIds in the cycle, or None
   */
  override def detectCycles[A](tasks: List[Task[A]]): Option[List[TaskId]] = {
    // Build a map of TaskId -> Task for easy lookup
    val taskMap: Map[TaskId, Task[A]] = tasks.map(t => t.id -> t).toMap

    // Track visited states: not in map = White, true = Gray, false = Black
    var visitedStates: Map[TaskId, Boolean] = Map.empty

    // Track the current DFS path (for reconstructing the cycle)
    var currentPath: List[TaskId] = List.empty

    /**
     * Performs DFS from a given task, looking for back edges (cycles).
     *
     * @param taskId The task to start DFS from
     * @return Some(cycle) if a cycle is found, None otherwise
     */
    def dfs(taskId: TaskId): Option[List[TaskId]] = {
      taskMap.get(taskId) match {
        case None =>
          // Task dependency doesn't exist in our task list
          // Skip it - will be caught by topological sort as unresolvable
          None

        case Some(task) =>
          visitedStates.get(taskId) match {
            case Some(true) =>
              // Gray node - we've found a back edge (cycle)!
              // Extract the cycle from currentPath
              val cycleStart = currentPath.indexOf(taskId)
              Some(currentPath.drop(cycleStart) :+ taskId)

            case Some(false) =>
              // Black node - already fully explored, no cycle here
              None

            case None =>
              // White node - not visited yet
              // Mark as Gray (currently visiting)
              visitedStates = visitedStates + (taskId -> true)
              currentPath = currentPath :+ taskId

              // Recursively visit all dependencies
              val cycleFound = task.dependencies.foldLeft(None: Option[List[TaskId]]) {
                (acc, depId) =>
                  acc match {
                    case Some(cycle) => Some(cycle) // Already found a cycle, propagate it
                    case None        => dfs(depId)  // Continue searching
                  }
              }

              // Mark as Black (done visiting)
              visitedStates = visitedStates + (taskId -> false)
              currentPath = currentPath.init

              cycleFound
          }
      }
    }

    // Try DFS from each unvisited task
    tasks.foldLeft(None: Option[List[TaskId]]) { (acc, task) =>
      acc match {
        case Some(cycle) => Some(cycle) // Already found a cycle
        case None =>
          if (visitedStates.contains(task.id)) {
            None // Already visited this task
          } else {
            dfs(task.id)
          }
      }
    }
  }

  // ============================================
  // PRIVATE HELPERS
  // ============================================

  /**
   * Performs topological sort using Kahn's algorithm.
   *
   * Algorithm:
   * 1. Calculate in-degrees (number of dependencies) for each task
   * 2. Start with tasks that have in-degree 0 (no dependencies)
   * 3. Process each task, decrementing in-degrees of dependent tasks
   * 4. When a task's in-degree reaches 0, add it to the queue
   * 5. Repeat until all tasks are processed
   *
   * @param tasks List of tasks to sort
   * @tparam A    The result type of the tasks
   * @return Either an error or the topologically sorted task list
   */
  private def topologicalSort[A](
      tasks: List[Task[A]]
  ): Either[SchedulingError, List[Task[A]]] = {
    // Build task map for easy lookup
    val taskMap: Map[TaskId, Task[A]] = tasks.map(t => t.id -> t).toMap

    // Calculate in-degrees (number of dependencies each task has)
    val inDegrees: Map[TaskId, Int] = calculateInDegrees(tasks)

    // Initialize queue with tasks that have no dependencies (in-degree 0)
    var queue: List[TaskId] = inDegrees.filter(_._2 == 0).keys.toList

    // Result accumulator
    var result: List[Task[A]] = List.empty

    // Mutable copy of in-degrees (we'll decrement as we process)
    var currentInDegrees = inDegrees

    // Process tasks in topological order
    while (queue.nonEmpty) {
      // Take first task from queue
      val taskId = queue.head
      queue = queue.tail

      // Add task to result
      taskMap.get(taskId) match {
        case Some(task) =>
          result = result :+ task

          // Find all tasks that depend on this one and decrement their in-degree
          tasks.foreach { dependentTask =>
            if (dependentTask.dependencies.contains(taskId)) {
              val newDegree = currentInDegrees.getOrElse(dependentTask.id, 0) - 1
              currentInDegrees = currentInDegrees + (dependentTask.id -> newDegree)

              // If in-degree becomes 0, task is ready to be scheduled
              if (newDegree == 0) {
                queue = queue :+ dependentTask.id
              }
            }
          }

        case None =>
          // Task not found - shouldn't happen, but handle gracefully
          ()
      }
    }

    // Verify all tasks were scheduled
    if (result.size == tasks.size) {
      Right(result)
    } else {
      // Some tasks couldn't be scheduled (missing dependencies)
      val scheduledIds = result.map(_.id).toSet
      val unscheduledIds = tasks.map(_.id).filterNot(scheduledIds.contains).toSet
      Left(UnresolvableDependencies(unscheduledIds))
    }
  }

  /**
   * Calculates in-degrees (number of dependencies) for each task.
   *
   * @param tasks List of tasks
   * @tparam A    The result type of the tasks
   * @return Map from TaskId to its in-degree
   */
  private def calculateInDegrees[A](tasks: List[Task[A]]): Map[TaskId, Int] = {
    tasks.map(task => task.id -> task.dependencies.size).toMap
  }
}