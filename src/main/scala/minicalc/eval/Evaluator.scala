package minicalc.eval

import cats.effect.IO
import scala.concurrent.duration._

import minicalc.ast._
import minicalc.execution.FiberExecutionEngine
import minicalc.scheduler.{CircularDependency, UnresolvableDependencies}
import minicalc.task._

/**
 * Core evaluation engine for MiniCalc expressions.
 *
 * Provides two evaluation modes:
 * - `eval`: Pure evaluation for simple expressions (no side effects)
 * - `evalIO`: Effectful evaluation for task-related expressions (uses IO monad)
 *
 * Design principles:
 * - Pure functions where possible (no side effects)
 * - Either for error handling (no exceptions)
 * - Immutable environments (functional style)
 * - Generic helper functions to eliminate code duplication
 */
object Evaluator {

  // ============================================
  // VALUE EXTRACTORS
  // ============================================

  /**
   * Extracts a number from a value, or returns a type mismatch error.
   * Used for arithmetic and comparison operations.
   *
   * @param v The value to extract from
   * @return Either the extracted Double or a TypeMismatch error
   */
  val extractNumber: Value => Either[EvalError, Double] = (v: Value) => {
    v.asNumber match {
      case Some(n) => Right(n)
      case None    => Left(TypeMismatch("Number", v.typeName, v.show))
    }
  }

  /**
   * Extracts a boolean from a value, or returns a type mismatch error.
   * Used for logical operations and if conditions.
   *
   * @param v The value to extract from
   * @return Either the extracted Boolean or a TypeMismatch error
   */
  val extractBoolean: Value => Either[EvalError, Boolean] = (v: Value) => {
    v.asBoolean match {
      case Some(b) => Right(b)
      case None    => Left(TypeMismatch("Boolean", v.typeName, v.show))
    }
  }

  // ============================================
  // PURE EVALUATION (no side effects)
  // ============================================

  /**
   * Generic binary operation evaluator with type parameters.
   *
   * This function handles all binary operations by abstracting over:
   * - Input type T (Double for arithmetic, Boolean for logical)
   * - Output type R (Double for arithmetic, Boolean for comparisons/logical)
   *
   * Strategy:
   * 1. Evaluate both operands
   * 2. Extract values using the provided extraction function
   * 3. Apply the operation
   *
   * @param left    Left operand expression
   * @param right   Right operand expression
   * @param env     Current environment for variable lookups
   * @param extract Function to extract and type-check values
   * @param op      Function to apply to the extracted values
   * @tparam T      Input type for operands
   * @tparam R      Result type of the operation
   * @return Either an error or the operation result
   */
  def evalBinaryOp[T, R](
      left: Expr,
      right: Expr,
      env: Environment,
      extract: Value => Either[EvalError, T],
      op: (T, T) => Either[EvalError, R]
  ): Either[EvalError, R] = {
    for {
      l      <- eval(left, env)
      r      <- eval(right, env)
      lv     <- extract(l)
      rv     <- extract(r)
      result <- op(lv, rv)
    } yield result
  }

  /**
   * Main pure evaluation function - recursively evaluates expressions to values.
   *
   * Handles simple expressions that don't require IO effects:
   * - Literals (numbers, booleans)
   * - Variables
   * - Let bindings
   * - Conditionals
   * - Binary operations (arithmetic, comparison, logical)
   *
   * Task-related expressions must use `evalIO` instead.
   *
   * @param expr The expression to evaluate
   * @param env  The environment containing variable bindings
   * @return Either an error or the evaluated value
   */
  def eval(expr: Expr, env: Environment): Either[EvalError, Value] = {
    expr match {
      // ============================================
      // LITERALS - Direct value conversion
      // ============================================

      case NumLit(value) =>
        Right(NumValue(value))

      case BoolLit(value) =>
        Right(BoolValue(value))

      // ============================================
      // VARIABLES - Environment lookup
      // ============================================

      case Var(name) =>
        env.get(name) match {
          case Some(value) => Right(value)
          case None        => Left(UndefinedVariable(name))
        }

      // ============================================
      // LET BINDINGS - Create new scope
      // ============================================

      case Let(name, value, body) =>
        for {
          v <- eval(value, env)
          b <- eval(body, env.add(name, v))
        } yield b

      // ============================================
      // CONDITIONALS - Branch based on condition
      // ============================================

      case If(cond, thenExpr, elseExpr) =>
        for {
          c <- eval(cond, env)
          b <- extractBoolean(c)
          result <- if (b) eval(thenExpr, env) else eval(elseExpr, env)
        } yield result

      // ============================================
      // BINARY OPERATIONS - Apply operators
      // ============================================

      case BinOp(op, left, right) =>
        op match {
          // Arithmetic operations: Number → Number
          case arithmeticOp: ArithmeticOp =>
            arithmeticOp match {
              case Add =>
                evalBinaryOp(left, right, env, extractNumber,
                  (l: Double, r: Double) => Right(l + r)
                ).map(NumValue(_))

              case Sub =>
                evalBinaryOp(left, right, env, extractNumber,
                  (l: Double, r: Double) => Right(l - r)
                ).map(NumValue(_))

              case Mul =>
                evalBinaryOp(left, right, env, extractNumber,
                  (l: Double, r: Double) => Right(l * r)
                ).map(NumValue(_))

              case Div =>
                evalBinaryOp(left, right, env, extractNumber,
                  (l: Double, r: Double) =>
                    if (r == 0.0) Left(DivisionByZero(s"$l / $r"))
                    else Right(l / r)
                ).map(NumValue(_))
            }

          // Comparison operations: Number → Boolean
          case comparisonOp: ComparisonOp =>
            comparisonOp match {
              case Lt =>
                evalBinaryOp(left, right, env, extractNumber,
                  (l: Double, r: Double) => Right(l < r)
                ).map(BoolValue(_))

              case Gt =>
                evalBinaryOp(left, right, env, extractNumber,
                  (l: Double, r: Double) => Right(l > r)
                ).map(BoolValue(_))

              case Lte =>
                evalBinaryOp(left, right, env, extractNumber,
                  (l: Double, r: Double) => Right(l <= r)
                ).map(BoolValue(_))

              case Gte =>
                evalBinaryOp(left, right, env, extractNumber,
                  (l: Double, r: Double) => Right(l >= r)
                ).map(BoolValue(_))

              case Eq =>
                evalBinaryOp(left, right, env, extractNumber,
                  (l: Double, r: Double) => Right(l == r)
                ).map(BoolValue(_))

              case Neq =>
                evalBinaryOp(left, right, env, extractNumber,
                  (l: Double, r: Double) => Right(l != r)
                ).map(BoolValue(_))
            }

          // Logical operations: Boolean → Boolean
          case logicalOp: LogicalOp =>
            logicalOp match {
              case And =>
                evalBinaryOp(left, right, env, extractBoolean,
                  (l: Boolean, r: Boolean) => Right(l && r)
                ).map(BoolValue(_))

              case Or =>
                evalBinaryOp(left, right, env, extractBoolean,
                  (l: Boolean, r: Boolean) => Right(l || r)
                ).map(BoolValue(_))
            }
        }

      // ============================================
      // TASK EXPRESSIONS - Require evalIO
      // ============================================

      case _: TaskDef      => Left(UnsupportedInPureEval("TaskDef"))
      case _: TaskExec     => Left(UnsupportedInPureEval("TaskExec"))
      case _: TaskSequence => Left(UnsupportedInPureEval("TaskSequence"))
      case _: TaskParallel => Left(UnsupportedInPureEval("TaskParallel"))
      case _: ScheduleExpr => Left(UnsupportedInPureEval("ScheduleExpr"))
      case _: WorkflowDef  => Left(UnsupportedInPureEval("WorkflowDef"))
    }
  }

  // ============================================
  // EFFECTFUL EVALUATION (with IO)
  // ============================================

  /**
   * Effectful evaluation function for expressions that may have side effects.
   *
   * Handles all expression types including task-related expressions that
   * require IO for execution. Simple expressions are delegated to the
   * pure `eval` function.
   *
   * @param expr The expression to evaluate
   * @param env  The environment containing variable bindings and task definitions
   * @return IO containing either an error or a tuple of (value, updated environment)
   */
  def evalIO(expr: Expr, env: Environment): IO[Either[EvalError, (Value, Environment)]] = {
    expr match {
      // ============================================
      // SIMPLE EXPRESSIONS - Delegate to pure eval
      // ============================================

      case NumLit(_)      => IO.pure(eval(expr, env).map(v => (v, env)))
      case BoolLit(_)     => IO.pure(eval(expr, env).map(v => (v, env)))
      case Var(_)         => IO.pure(eval(expr, env).map(v => (v, env)))
      case BinOp(_, _, _) => IO.pure(eval(expr, env).map(v => (v, env)))

      // ============================================
      // LET BINDINGS - Chain evalIO calls
      // ============================================

      case Let(name, value, body) =>
        evalIO(value, env).flatMap {
          case Left(err)           => IO.pure(Left(err))
          case Right((val1, env1)) => evalIO(body, env1.add(name, val1))
        }

      // ============================================
      // CONDITIONALS - Chain evalIO calls
      // ============================================

      case If(cond, thenExpr, elseExpr) =>
        evalIO(cond, env).flatMap {
          case Left(err) => IO.pure(Left(err))
          case Right((condValue, env1)) =>
            condValue.asBoolean match {
              case Some(true)  => evalIO(thenExpr, env1)
              case Some(false) => evalIO(elseExpr, env1)
              case None        => IO.pure(Left(TypeMismatch("Boolean", condValue.typeName, condValue.show)))
            }
        }

      // ============================================
      // TASK DEFINITION - Build and register task
      // ============================================

      case TaskDef(name, action, priority, dependencies, timeout, retryConfig) =>
        // Step 1: Create the IO action from the expression
        val taskAction: IO[Value] = IO.defer {
          evalIO(action, env).flatMap {
            case Right((value, _)) => IO.pure(value)
            case Left(err)         => IO.raiseError(new RuntimeException(err.message))
          }
        }

        // Step 2: Build the task with base configuration
        val baseBuilder = TaskBuilder.taskIO(name)(taskAction)
          .withId(TaskId.fromName(name))
          .withPriority(
            priority match {
              case Some(Var(p)) => Priority.fromString(p).getOrElse(Normal)
              case _            => Normal
            }
          )
          .dependsOn(dependencies.map(TaskId.fromName): _*)

        // Step 3: Add timeout if specified
        val builderWithTimeout = timeout match {
          case Some(timeoutExpr) =>
            eval(timeoutExpr, env) match {
              case Right(value) =>
                value.asNumber match {
                  case Some(seconds) => baseBuilder.withTimeout(seconds.seconds)
                  case None          => baseBuilder
                }
              case Left(_) => baseBuilder
            }
          case None => baseBuilder
        }

        // Step 4: Add retry policy if specified
        val taskWithRetry = retryConfig match {
          case Some(config) =>
            if (config.exponential)
              builderWithTimeout.withExponentialRetry(config.maxAttempts, config.backoffSeconds.seconds)
            else
              builderWithTimeout.withRetry(config.maxAttempts, config.backoffSeconds.seconds)
          case None => builderWithTimeout
        }

        // Step 5: Build the task and add to environment
        val task = taskWithRetry.build
        IO.pure(Right((TaskValue(task), env.addTask(name, task).add(name, TaskValue(task)))))

      // ============================================
      // TASK EXECUTION - Run a single task
      // ============================================

      case TaskExec(taskName) =>
        env.getTask(taskName) match {
          case None =>
            // Task doesn't exist - return proper error
            IO.pure(Left(TaskNotFound(taskName)))

          case Some(task) =>
            // Check if dependencies exist
            val missingDeps = task.dependencies
              .filterNot(depId => env.tasks.contains(depId.value))
              .map(_.value)
              .toList

            if (missingDeps.nonEmpty) {
              // Some dependencies are missing
              IO.pure(Left(UnresolvedDependencies(taskName, missingDeps)))
            } else {
              // All dependencies exist - execute
              val engine = new FiberExecutionEngine(env.scheduler)
              engine.execute(List(task), env.executionConfig).map { report =>
                report.results.headOption match {
                  case Some(result: Success[_]) =>
                    Right((TaskResultValue(result.asInstanceOf[TaskResult[Value]]), env))

                  case Some(result: Failure[_]) =>
                    // Task execution failed - convert to EvalError
                    Left(TaskExecutionFailed(taskName, result.error.message))

                  case Some(result: Skipped[_]) =>
                    // Task was skipped
                    Left(TaskExecutionFailed(taskName, s"Task was skipped: ${result.reason}"))

                  case None =>
                    // No results at all - shouldn't happen
                    Left(InternalError(s"Task execution for '$taskName' returned no results"))
                }
              }.handleErrorWith { error =>
                // Convert uncaught exceptions to EvalErrors
                IO.pure(Left(TaskExecutionFailed(taskName, error.getMessage)))
              }
            }
        }

      // ============================================
      // TASK SEQUENCE - Collect tasks for sequential execution
      // ============================================

      case TaskSequence(taskExprs) =>
        collectTasks(taskExprs, env).map {
          case Left(err)                   => Left(err)
          case Right((taskList, finalEnv)) => Right((TaskListValue(taskList), finalEnv))
        }

      // ============================================
      // TASK PARALLEL - Collect tasks for parallel execution
      // ============================================

      case TaskParallel(taskExprs) =>
        collectTasks(taskExprs, env).map {
          case Left(err)                   => Left(err)
          case Right((taskList, finalEnv)) => Right((TaskListValue(taskList), finalEnv))
        }

      // ============================================
      // SCHEDULE EXPRESSION - Execute tasks with scheduler
      // ============================================

      case ScheduleExpr(tasksExpr, strategy) =>
        evalIO(tasksExpr, env).flatMap {
          case Left(err) =>
            IO.pure(Left(err))

          case Right((value, newEnv)) =>
            value.asTaskList match {
              case None =>
                IO.pure(Left(TypeMismatch("TaskList", value.typeName, value.show)))

              case Some(taskList) if taskList.isEmpty =>
                // Empty task list - nothing to schedule
                IO.pure(Right((TaskListValue(List.empty), newEnv)))

              case Some(taskList) =>
                // First, check for scheduling errors (cycles, missing deps)
                newEnv.scheduler.schedule(taskList, strategy).flatMap {
                  case Left(CircularDependency(cycle)) =>
                    // Convert scheduler error to EvalError
                    IO.pure(Left(CircularDependencyDetected(cycle.map(_.value))))

                  case Left(UnresolvableDependencies(taskIds)) =>
                    // Convert scheduler error to EvalError
                    val taskName = taskList.headOption.map(_.name).getOrElse("unknown")
                    IO.pure(Left(UnresolvedDependencies(taskName, taskIds.map(_.value).toList)))

                  case Right(orderedTasks) =>
                    // Scheduling succeeded - execute the tasks
                    val engine = new FiberExecutionEngine(newEnv.scheduler)
                    engine.execute(orderedTasks, newEnv.executionConfig).map { report =>
                      // Check for any failures
                      val failures = report.results.collect { case f: Failure[_] => f }

                      if (failures.nonEmpty) {
                        // At least one task failed
                        val firstFailure = failures.head
                        Left(TaskExecutionFailed(
                          firstFailure.taskId.value,
                          firstFailure.error.message
                        ))
                      } else {
                        // All tasks succeeded (or were skipped)
                        Right((
                          ExecutionReportValue(
                            report.totalTasks,
                            report.successful,
                            report.failed,
                            report.results.asInstanceOf[List[TaskResult[Value]]]
                          ),
                          newEnv
                        ))
                      }
                    }
                }
            }
        }

      // ============================================
      // WORKFLOW DEFINITION - Register workflow
      // ============================================

      case WorkflowDef(name, body) =>
        val newEnv = env.addWorkflow(name, body)
        IO.pure(Right((WorkflowValue(name, body), newEnv)))
    }
  }

  // ============================================
  // HELPER FUNCTIONS
  // ============================================

  /**
   * Collects tasks from a list of expressions.
   *
   * Evaluates each expression in sequence, extracting Task values and
   * threading the environment through. Used by TaskSequence and TaskParallel.
   *
   * @param taskExprs List of expressions that should evaluate to TaskValues
   * @param env       Initial environment
   * @return IO containing either an error or a tuple of (task list, final environment)
   */
  private def collectTasks(
      taskExprs: List[Expr],
      env: Environment
  ): IO[Either[EvalError, (List[Task[Value]], Environment)]] = {
    val initialAccumulator: IO[Either[EvalError, (List[Task[Value]], Environment)]] =
      IO.pure(Right((List.empty[Task[Value]], env)))

    taskExprs.foldLeft(initialAccumulator) { (accIO, expr) =>
      accIO.flatMap {
        case Left(error) =>
          IO.pure(Left(error))

        case Right((accumulatedTasks, currentEnv)) =>
          evalIO(expr, currentEnv).map {
            case Left(error) =>
              Left(error)

            case Right((value, newEnv)) =>
              value.asTask match {
                case Some(task) =>
                  Right((accumulatedTasks :+ task, newEnv))
                case None =>
                  Left(TypeMismatch("Task", value.typeName, value.show))
              }
          }
      }
    }
  }
}