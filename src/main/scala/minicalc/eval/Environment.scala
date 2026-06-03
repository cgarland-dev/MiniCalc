package minicalc.eval

import cats.effect.IO

import minicalc.ast.{Expr, Value}
import minicalc.execution.ExecutionConfig
import minicalc.monitoring.{InMemoryMonitor, TaskMonitor}
import minicalc.repl.REPLSettings
import minicalc.scheduler.{Scheduler, TopologicalScheduler}
import minicalc.task.Task

/**
 * Immutable environment for variable bindings and task management.
 *
 * Stores variable name -> value mappings, task definitions, and workflow definitions.
 * All operations return new Environment instances rather than modifying in place
 * (functional style).
 *
 * The environment also holds references to the scheduler, monitor, execution
 * configuration, and REPL settings needed for task orchestration.
 *
 * @param bindings        Map of variable names to their values
 * @param metadata        Optional metadata for future extensibility
 * @param tasks           Map of task names to Task objects
 * @param workflows       Map of workflow names to their body expressions
 * @param scheduler       Scheduler instance for task ordering
 * @param monitor         TaskMonitor for tracking execution statistics
 * @param executionConfig Configuration for task execution (parallelism, etc.)
 * @param settings        REPL settings (continue-on-error, verbose, show-timing)
 */
case class Environment(
    bindings: Map[String, Value],
    metadata: Map[String, String],
    tasks: Map[String, Task[Value]],
    workflows: Map[String, Expr],
    scheduler: Scheduler,
    monitor: TaskMonitor,
    executionConfig: ExecutionConfig,
    settings: REPLSettings
) {

  // ============================================
  // VARIABLE BINDINGS
  // ============================================

  /**
   * Adds a variable binding, returning a new Environment.
   *
   * If the variable already exists, it will be shadowed by the new value.
   *
   * @param name  Variable name to bind
   * @param value Value to bind to the name
   * @return New environment with the binding added
   */
  def add(name: String, value: Value): Environment = {
    this.copy(bindings = bindings + (name -> value))
  }

  /**
   * Looks up a variable in the environment.
   *
   * @param name Variable name to look up
   * @return Some(value) if found, None if not found
   */
  def get(name: String): Option[Value] = {
    bindings.get(name)
  }

  // ============================================
  // METADATA
  // ============================================

  /**
   * Adds metadata, returning a new Environment.
   *
   * Metadata is preserved across variable additions and can be used
   * for debugging, tracing, or other auxiliary information.
   *
   * @param key   Metadata key
   * @param value Metadata value
   * @return New environment with metadata added
   */
  def addMetadata(key: String, value: String): Environment = {
    this.copy(metadata = metadata + (key -> value))
  }

  // ============================================
  // TASK MANAGEMENT
  // ============================================

  /**
   * Adds a task to the environment.
   *
   * @param name Task name (used for lookup and dependency resolution)
   * @param task Task object containing the action and configuration
   * @return New environment containing the new task
   */
  def addTask(name: String, task: Task[Value]): Environment = {
    this.copy(tasks = tasks + (name -> task))
  }

  /**
   * Retrieves a task from the environment by name.
   *
   * @param name Task name to look up
   * @return Some(task) if it exists, None otherwise
   */
  def getTask(name: String): Option[Task[Value]] = {
    tasks.get(name)
  }

  /**
   * Returns a list of all defined task names.
   *
   * @return List of task names
   */
  def listTasks: List[String] = {
    tasks.keys.toList
  }

  /**
   * Removes all tasks from the environment.
   *
   * @return New environment with an empty task container
   */
  def clearTasks: Environment = {
    this.copy(tasks = Map.empty)
  }

  // ============================================
  // WORKFLOW MANAGEMENT
  // ============================================

  /**
   * Adds a workflow to the environment.
   *
   * A workflow is a named expression (typically a sequence, parallel,
   * or schedule expression) that can be executed later via :run command.
   *
   * @param name Name of the workflow
   * @param expr Body expression of the workflow
   * @return New environment containing the new workflow
   */
  def addWorkflow(name: String, expr: Expr): Environment = {
    this.copy(workflows = workflows + (name -> expr))
  }

  /**
   * Retrieves a workflow from the environment by name.
   *
   * @param name Workflow name to look up
   * @return Some(expr) if it exists, None otherwise
   */
  def getWorkflow(name: String): Option[Expr] = {
    workflows.get(name)
  }

  // ============================================
  // SETTINGS MANAGEMENT
  // ============================================

  /**
   * Updates the REPL settings.
   *
   * @param newSettings The new settings to use
   * @return New environment with updated settings
   */
  def withSettings(newSettings: REPLSettings): Environment = {
    this.copy(settings = newSettings)
  }
}

/**
 * Companion object with factory methods for Environment.
 */
object Environment {

  /**
   * Creates a new environment with initialized scheduler, monitor, config, and settings.
   *
   * This is an IO action because the monitor uses a Ref for thread-safe
   * state management, which requires IO to create.
   *
   * @return IO effect that produces an empty, fully initialized environment
   */
  def create: IO[Environment] = {
    for {
      monitor <- InMemoryMonitor.create
    } yield Environment(
      bindings = Map.empty,
      metadata = Map.empty,
      tasks = Map.empty,
      workflows = Map.empty,
      scheduler = new TopologicalScheduler,
      monitor = monitor,
      executionConfig = ExecutionConfig.default,
      settings = REPLSettings.default
    )
  }
}