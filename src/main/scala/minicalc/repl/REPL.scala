package minicalc.repl

import scala.concurrent.duration._

import cats.effect.{ExitCode, IO, IOApp}

import minicalc.ast.Value
import minicalc.eval.{Environment, Evaluator, ParseError, WorkflowNotFound}
import minicalc.parser.Parser
import minicalc.task._

/**
 * Read-Eval-Print Loop (REPL) for interactive MiniCalc sessions.
 *
 * Provides an interactive command-line interface for:
 * - Evaluating MiniCalc expressions
 * - Defining and executing tasks
 * - Defining and running workflows
 * - Viewing execution statistics
 * - Configuring REPL behavior with :set command
 *
 * The REPL uses Cats Effect IO for all side effects, making the
 * core logic testable and the control flow explicit.
 */
object REPL extends IOApp {

  // ============================================
  // FORMATTING HELPERS
  // ============================================

  /**
   * Formats a task for display in the :tasks command.
   *
   * Shows the task name along with its configuration:
   * priority, dependencies, timeout, and retry policy.
   *
   * @param name The task's registered name
   * @param task The task object
   * @return Formatted string representation
   */
  private def formatTask(name: String, task: Task[Value]): String = {
    val priorityStr = task.priority.toString

    val depsStr = if (task.dependencies.isEmpty) {
      "none"
    } else {
      task.dependencies.map(_.value).mkString(", ")
    }

    val timeoutStr = task.timeout match {
      case Some(duration) => duration.toString
      case None           => "none"
    }

    val retryStr = task.retryPolicy match {
      case NoRetry                              => "none"
      case LinearRetry(maxAttempts, delay)      => s"$maxAttempts attempts (linear, $delay)"
      case ExponentialRetry(maxAttempts, delay) => s"$maxAttempts attempts (exponential, $delay)"
    }

    s"$name [Priority: $priorityStr, Dependencies: $depsStr, Timeout: $timeoutStr, Retry: $retryStr]"
  }

  /**
   * Displays help text with available commands and syntax examples.
   *
   * @return The help text string
   */
  private def showHelp(): String = {
    HelpText.help
  }

  /**
   * Displays current variable bindings in the environment.
   *
   * @param env Environment to display
   * @return Formatted string showing all bindings, or a message if empty
   */
  private def showEnvironment(env: Environment): String = {
    if (env.bindings.isEmpty) {
      "Environment is empty"
    } else {
      env.bindings
        .map { case (name, value) => s"$name = ${value.show}" }
        .mkString("\n")
    }
  }

  /**
   * Formats a parse error with context from the original input.
   *
   * Shows the error message along with the relevant line and a
   * caret pointing to the error position.
   *
   * @param error The parse error
   * @param input The original input string
   * @return Formatted error message with context
   */
  private def formatParseError(error: ParseError, input: String): String = {
    error.messageWithContext(input)
  }

  /**
   * Parses a :set command and returns the updated settings.
   *
   * Supports:
   * - :set continue-on-error true|false
   * - :set verbose true|false
   * - :set show-timing true|false
   *
   * @param args    The arguments after ":set"
   * @param current Current settings
   * @return Either an error message or the updated settings
   */
  private def parseSetCommand(args: String, current: REPLSettings): Either[String, REPLSettings] = {
    args.trim.split("\\s+").toList match {
      case settingName :: value :: Nil =>
        val boolValue = value.toLowerCase match {
          case "true"  => Some(true)
          case "false" => Some(false)
          case "on"    => Some(true)
          case "off"   => Some(false)
          case "1"     => Some(true)
          case "0"     => Some(false)
          case _       => None
        }

        boolValue match {
          case None =>
            Left(s"Invalid value '$value'. Expected: true, false, on, off, 1, or 0")

          case Some(b) =>
            settingName.toLowerCase match {
              case "continue-on-error" => Right(current.copy(continueOnError = b))
              case "verbose"           => Right(current.copy(verbose = b))
              case "show-timing"       => Right(current.copy(showTiming = b))
              case _ =>
                Left(s"Unknown setting '$settingName'. Available: continue-on-error, verbose, show-timing")
            }
        }

      case _ =>
        Left("Usage: :set <setting> <value>\nExample: :set continue-on-error true")
    }
  }

  // ============================================
  // INPUT PROCESSING
  // ============================================

  /**
   * Processes a single line of input from the user.
   *
   * Handles both special commands (prefixed with ':') and expression
   * evaluation. Returns an IO action that produces the new environment
   * and output string to display.
   *
   * @param input User input string
   * @param env   Current environment
   * @return IO containing (new environment, output string)
   */
  def processInputIO(input: String, env: Environment): IO[(Environment, String)] = {
    // Capture start time if show-timing is enabled
    val timedExecution: IO[(Environment, String)] => IO[(Environment, String)] = { action =>
      if (env.settings.showTiming) {
        for {
          startTime       <- IO.realTime
          result          <- action
          endTime         <- IO.realTime
          (newEnv, output) = result
          duration         = endTime - startTime
          timedOutput      = if (output.nonEmpty) s"$output\n(completed in $duration)" else s"(completed in $duration)"
        } yield (newEnv, timedOutput)
      } else {
        action
      }
    }

    input.trim match {
      // ============================================
      // EMPTY INPUT
      // ============================================

      case "" =>
        IO.pure((env, ""))

      // ============================================
      // HELP AND INFO COMMANDS
      // ============================================

      case ":help" =>
        IO.pure((env, showHelp()))

      // ============================================
      // DEMO COMMAND
      // ============================================

      case ":demo" =>
        IO.println("Running MiniCalc demonstration...").flatMap { _ =>
          Demo.run(env).map(newEnv => (newEnv, "Demo complete!"))
        }

      case cmd if cmd.startsWith(":help ") =>
        val arg = cmd.stripPrefix(":help ").trim
        arg match {
          case "full" => IO.pure((env, HelpText.helpFull))
          case _ => IO.pure((env, "Unknown help topic. Try ':help' or ':help full'"))
        }

      case ":env" =>
        IO.pure((env, showEnvironment(env)))

      case ":tasks" =>
        val taskMap = env.tasks
        val output = if (taskMap.isEmpty) {
          "No tasks defined"
        } else {
          taskMap
            .map { case (name, task) => formatTask(name, task) }
            .mkString("\n")
        }
        IO.pure((env, output))

      case ":workflows" =>
        val workflowList = env.workflows.keys.toList
        val output = if (workflowList.isEmpty) {
          "No workflows defined"
        } else {
          workflowList.mkString("\n")
        }
        IO.pure((env, output))

      case ":stats" =>
        env.monitor.getStatistics.map { stats =>
          val output =
            s"""Execution Statistics:
               |  Tasks Started:   ${stats.tasksStarted}
               |  Tasks Completed: ${stats.tasksCompleted}
               |  Tasks Failed:    ${stats.tasksFailed}
               |  Average Duration: ${stats.averageDuration}
               |  Currently Running: ${stats.currentlyRunning.size}""".stripMargin
          (env, output)
        }

      // ============================================
      // SETTINGS COMMANDS
      // ============================================

      case ":settings" =>
        IO.pure((env, env.settings.show))

      case cmd if cmd.startsWith(":set ") =>
        val args = cmd.stripPrefix(":set ")
        parseSetCommand(args, env.settings) match {
          case Right(newSettings) =>
            val newEnv = env.withSettings(newSettings)
            IO.pure((newEnv, s"Setting updated. ${newSettings.show}"))
          case Left(errorMsg) =>
            IO.pure((env, s"Error: $errorMsg"))
        }

      // ============================================
      // STATE MODIFICATION COMMANDS
      // ============================================

      case ":clear" =>
        Environment.create.map(newEnv => (newEnv, "Environment cleared"))

      case ":clear-tasks" =>
        IO.pure((env.clearTasks, "Tasks cleared"))

      case ":reset-stats" =>
        env.monitor.reset.map(_ => (env, "Statistics reset"))

      // ============================================
      // WORKFLOW EXECUTION
      // ============================================

      case cmd if cmd.startsWith(":run ") =>
        val workflowName = cmd.stripPrefix(":run ").trim
        env.getWorkflow(workflowName) match {
          case Some(body) =>
            timedExecution {
              Evaluator.evalIO(body, env).map {
                case Right((value, newEnv)) => (newEnv, value.show)
                case Left(err)              => (env, s"Error: ${err.message}")
              }
            }
          case None =>
            IO.pure((env, s"Error: ${WorkflowNotFound(workflowName).message}"))
        }

      // ============================================
      // EXPRESSION EVALUATION
      // ============================================

      case _ =>
        Parser.parse(input) match {
          case Right(expr) =>
            timedExecution {
              Evaluator.evalIO(expr, env).map {
                case Right((value, newEnv)) =>
                  // Verbose output if enabled
                  val output = if (env.settings.verbose) {
                    s"${value.show} : ${value.typeName}"
                  } else {
                    value.show
                  }
                  (newEnv, output)
                case Left(err) =>
                  (env, s"Error: ${err.message}")
              }
            }
          case Left(parseError) =>
            IO.pure((env, formatParseError(parseError, input)))
        }
    }
  }

  // ============================================
  // MAIN LOOP
  // ============================================

  /**
   * Entry point for the REPL application.
   *
   * Initializes the environment and starts the interactive loop.
   *
   * @param args Command-line arguments (currently unused)
   * @return IO containing the exit code
   */
  def run(args: List[String]): IO[ExitCode] = {
    for {
      _   <- IO.println("Welcome to MiniCalc!")
      _   <- IO.println("Type :help for help, :quit to exit")
      env <- Environment.create
      _   <- loop(env)
    } yield ExitCode.Success
  }

  /**
   * Main REPL loop - reads input, processes it, displays output, and repeats.
   *
   * The loop continues until the user enters :quit or :q, or until
   * end-of-input is reached (null from readLine).
   *
   * Uses Option[Environment] to signal whether to continue:
   * - Some(env) means continue with the new environment
   * - None means exit the loop
   *
   * @param env Current environment
   * @return IO[Unit] representing the loop execution
   */
  def loop(env: Environment): IO[Unit] = {
    for {
      _     <- IO.print("> ")
      input <- IO.readLine

      result <- {
        if (input == null || input.trim == ":quit" || input.trim == ":q") {
          IO.println("Goodbye!").map(_ => None)
        } else {
          processInputIO(input, env).flatMap { case (newEnv, output) =>
            val printIO = if (output.nonEmpty) IO.println(output) else IO.unit
            printIO.map(_ => Some(newEnv))
          }
        }
      }

      _ <- result match {
        case Some(newEnv) => loop(newEnv)
        case None         => IO.unit
      }
    } yield ()
  }
}

// ============================================
// HELP TEXT
// ============================================

/**
 * Contains the help text displayed by the :help command.
 *
 * Separated into its own object for maintainability and to keep
 * the main REPL object focused on logic.
 */
object HelpText {

  val helpShort: String =
    "================================================================================\n" +
    "|                   MiniCalc REPL - Commands and Syntax                        |\n" +
    "================================================================================\n" +
    "|                                                                              |\n" +
    "| COMMANDS                                                                     |\n" +
    "| --------                                                                     |\n" +
    "| :help              Show this help message                                    |\n" +
    "| :demo              Run interactive demonstration of all features             |\n" +
    "| :help full         Show detailed help with examples                          |\n" +
    "| :env               List current variable bindings                            |\n" +
    "| :clear             Clear all definitions (variables, tasks, workflows)       |\n" +
    "| :quit, :q          Exit MiniCalc                                             |\n" +
    "|                                                                              |\n" +
    "| :tasks             Show all defined tasks with their configuration           |\n" +
    "| :clear-tasks       Clear all task definitions                                |\n" +
    "| :workflows         Show all defined workflows                                |\n" +
    "| :run <name>        Execute a workflow by name                                |\n" +
    "|                                                                              |\n" +
    "| :stats             Show execution statistics                                 |\n" +
    "| :reset-stats       Reset execution statistics                                |\n" +
    "|                                                                              |\n" +
    "| :settings          Show current REPL settings                                |\n" +
    "| :set <name> <val>  Change a setting (true/false, on/off, 1/0)                |\n" +
    "|                                                                              |\n" +
    "| QUICK START                                                                  |\n" +
    "| -----------                                                                  |\n" +
    "| > 1 + 2 * 3                  Arithmetic                                      |\n" +
    "| > let x = 5 in x * 2         Variables                                       |\n" +
    "| > define task t1 = 42        Define a task                                   |\n" +
    "| > execute t1                 Execute a task                                  |\n" +
    "|                                                                              |\n" +
    "| Type ':help full' for detailed syntax and examples                           |\n" +
    "|                                                                              |\n" +
    "================================================================================"

  val helpFull: String = 
    "================================================================================\n" +
    "|                   MiniCalc REPL - Commands and Syntax                        |\n" +
    "================================================================================\n" +
    "|                                                                              |\n" +
    "| COMMANDS                                                                     |\n" +
    "| --------                                                                     |\n" +
    "| :help              Show this help message                                    |\n" +
    "| :demo              Run interactive demonstration of all features             |\n" +
    "| :env               List current variable bindings                            |\n" +
    "| :clear             Clear all definitions (variables, tasks, workflows)       |\n" +
    "| :quit, :q          Exit MiniCalc                                             |\n" +
    "|                                                                              |\n" +
    "| :tasks             Show all defined tasks with their configuration           |\n" +
    "| :clear-tasks       Clear all task definitions                                |\n" +
    "| :workflows         Show all defined workflows                                |\n" +
    "| :run <name>        Execute a workflow by name                                |\n" +
    "|                                                                              |\n" +
    "| :stats             Show execution statistics                                 |\n" +
    "| :reset-stats       Reset execution statistics                                |\n" +
    "|                                                                              |\n" +
    "| :settings          Show current REPL settings                                |\n" +
    "| :set <name> <val>  Change a setting (true/false, on/off, 1/0)                |\n" +
    "|                                                                              |\n" +
    "| SETTINGS                                                                     |\n" +
    "| --------                                                                     |\n" +
    "| continue-on-error  Continue executing tasks even when some fail              |\n" +
    "| verbose            Show type information with values                         |\n" +
    "| show-timing        Show execution time for operations                        |\n" +
    "|                                                                              |\n" +
    "| EXPRESSIONS                                                                  |\n" +
    "| -----------                                                                  |\n" +
    "| 1 + 2 * 3                    Arithmetic (evaluates to 7)                     |\n" +
    "| true && false                Logical operations                              |\n" +
    "| 5 > 3                        Comparisons (evaluates to true)                 |\n" +
    "| if x > 0 then x else 0       Conditionals                                    |\n" +
    "| let x = 10 in x * 2          Local variable bindings                         |\n" +
    "|                                                                              |\n" +
    "| TASK DEFINITIONS                                                             |\n" +
    "| ----------------                                                             |\n" +
    "| define task myTask = <expr>                                                  |\n" +
    "|     with priority High|Normal|Low       (optional)                           |\n" +
    "|     depends on task1, task2             (optional)                           |\n" +
    "|     timeout 30                          (optional, in seconds)               |\n" +
    "|     retry 3 backoff 5                   (optional)                           |\n" +
    "|     exponential = true                  (optional, default false)            |\n" +
    "|                                                                              |\n" +
    "| TASK EXECUTION                                                               |\n" +
    "| --------------                                                               |\n" +
    "| execute myTask               Execute a single task                           |\n" +
    "| sequence [t1, t2, t3]        Execute tasks sequentially                      |\n" +
    "| parallel [t1, t2, t3]        Execute tasks in parallel                       |\n" +
    "| schedule <expr> with FIFO    Schedule with FIFO strategy                     |\n" +
    "| schedule <expr> with PRIORITY Schedule with priority strategy                |\n" +
    "|                                                                              |\n" +
    "| WORKFLOWS                                                                    |\n" +
    "| ---------                                                                    |\n" +
    "| define workflow myFlow = sequence [task1, task2]                             |\n" +
    "| :run myFlow                  Execute the workflow                            |\n" +
    "|                                                                              |\n" +
    "| EXAMPLES                                                                     |\n" +
    "| --------                                                                     |\n" +
    "| # Basic arithmetic and variables                                             |\n" +
    "| > let x = 5 in x * 2                                                         |\n" +
    "| 10.0                                                                         |\n" +
    "|                                                                              |\n" +
    "| > if 10 > 5 then true else false                                             |\n" +
    "| true                                                                         |\n" +
    "|                                                                              |\n" +
    "| # Task definition with all features                                          |\n" +
    "| > define task fetchData = 42 with priority High timeout 30 retry 3 backoff 5 |\n" +
    "| Task(fetchdata-...)                                                          |\n" +
    "|                                                                              |\n" +
    "| > define task processData = 100 depends on fetchData                         |\n" +
    "| Task(processdata-...)                                                        |\n" +
    "|                                                                              |\n" +
    "| > :tasks                                                                     |\n" +
    "| fetchData [Priority: High, Dependencies: none, Timeout: 30 seconds,          |\n" +
    "|            Retry: 3 attempts (linear, 5 seconds)]                            |\n" +
    "| processData [Priority: Normal, Dependencies: fetchData, Timeout: none,       |\n" +
    "|              Retry: none]                                                    |\n" +
    "|                                                                              |\n" +
    "| # Task execution                                                             |\n" +
    "| > execute fetchData                                                          |\n" +
    "| Task fetchData succeeded with 42.0 in ...                                    |\n" +
    "|                                                                              |\n" +
    "| # Workflow creation and execution                                            |\n" +
    "| > define workflow pipeline =                                                 |\n" +
    "|                           sequence [execute fetchData, execute processData]  |\n" +
    "| Workflow(pipeline)                                                           |\n" +
    "|                                                                              |\n" +
    "| > :run pipeline                                                              |\n" +
    "| TaskList(2 tasks)                                                            |\n" +
    "|                                                                              |\n" +
    "| # REPL settings                                                              |\n" +
    "| > :set verbose true                                                          |\n" +
    "| Setting updated. REPL Settings:                                              |\n" +
    "|   continue-on-error = false                                                  |\n" +
    "|   verbose           = true                                                   |\n" +
    "|   show-timing       = false                                                  |\n" +
    "|                                                                              |\n" +
    "| > 42                                                                         |\n" +
    "| 42.0 : Number                                                                |\n" +
    "|                                                                              |\n" +
    "| > :set show-timing true                                                      |\n" +
    "| Setting updated. ...                                                         |\n" +
    "|                                                                              |\n" +
    "| > 1 + 2                                                                      |\n" +
    "| 3.0                                                                          |\n" +
    "| (completed in 1.2ms)                                                         |\n" +
    "|                                                                              |\n" +
    "| # Advanced: Parallel execution with priorities                               |\n" +
    "| > define task highPrio = 1 with priority High                                |\n" +
    "| > define task lowPrio = 2 with priority Low                                  |\n" +
    "| > schedule parallel [execute highPrio, execute lowPrio] with PRIORITY        |\n" +
    "| TaskList(2 tasks)                                                            |\n" +
    "|                                                                              |\n" +
    "| # Statistics monitoring                                                      |\n" +
    "| > :stats                                                                     |\n" +
    "| Execution Statistics:                                                        |\n" +
    "|   Tasks Started:   4                                                         |\n" +
    "|   Tasks Completed: 4                                                         |\n" +
    "|   Tasks Failed:    0                                                         |\n" +
    "|   Average Duration: 150 milliseconds                                         |\n" +
    "|   Currently Running: 0                                                       |\n" +
    "|                                                                              |\n" +
    "================================================================================"
      
  def help: String = helpShort
}

/**
 * Demo script that showcases all MiniCalc features.
 */
object Demo {
  
  val commands: List[String] = List(
    // Header
    "# ===== MiniCalc Feature Demonstration =====",
    "",
    
    // Basic arithmetic
    "# 1. Arithmetic with operator precedence",
    "1 + 2 * 3",
    "(1 + 2) * 3",
    "",
    
    // Variables and let bindings
    "# 2. Variables and let bindings",
    "let x = 10 in x * 2",
    "let x = 5 in let y = 3 in x + y",
    "",
    
    // Conditionals
    "# 3. Conditionals",
    "if 10 > 5 then true else false",
    "let x = 7 in if x > 5 then x * 2 else x",
    "",
    
    // Logic and comparisons
    "# 4. Comparisons and logical operations",
    "5 > 3 && 2 < 4",
    "true || false && false",
    "",
    
    // Task definitions
    "# 5. Task definitions with various features",
    "define task simple = 42",
    "define task highPrio = 100 with priority High",
    "define task withTimeout = 50 timeout 30",
    "define task withRetry = 99 retry 3 backoff 5",
    ":tasks",
    "",
    
    // Task dependencies
    "# 6. Tasks with dependencies",
    "define task step1 = 10",
    "define task step2 = 20 depends on step1",
    "define task step3 = 30 depends on step1, step2",
    "",
    
    // Task execution
    "# 7. Task execution",
    "execute simple",
    "execute step3",
    "",
    
    // Workflows
    "# 8. Workflow creation and execution",
    "define workflow demo_flow = schedule sequence [step1, step2, step3] with FIFO",
    ":workflows",
    ":run demo_flow",
    "",
    
    // Parallel execution
    "# 9. Parallel task execution",
    "define task parallel1 = 1 with priority High",
    "define task parallel2 = 2 with priority Low",
    "schedule parallel [parallel1, parallel2] with PRIORITY",
    "",
    
    // REPL settings
    "# 10. REPL settings demonstration",
    ":set verbose true",
    "42",
    ":set verbose false",
    ":set show-timing true",
    "1 + 1",
    ":set show-timing false",
    "",
    
    // Statistics
    "# 11. Execution statistics",
    ":stats",
    "",
    
    // Summary
    "# ===== Demo Complete! =====",
    "# Type :help for more information",
    "# Type :clear to reset the environment"
  )
  
  /**
   * Runs the demo, executing each command and displaying results.
   */
  def run(initialEnv: Environment): IO[Environment] = {
    commands.foldLeft(IO.pure(initialEnv)) { (envIO, command) =>
      envIO.flatMap { env =>
        if (command.startsWith("#") || command.isEmpty) {
          // Comment or empty line - just print it
          IO.println(command).map(_ => env)
        } else {
          // Execute the command
          IO.print(s"> $command\n") *>
            REPL.processInputIO(command, env).flatMap { case (newEnv, output) =>
              (if (output.nonEmpty) IO.println(output) else IO.unit) *>
                IO.sleep(500.milliseconds) *>
                IO.pure(newEnv)
            }
        }
      }
    }
  }
}