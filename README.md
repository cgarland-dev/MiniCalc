# MiniCalc

A purely functional **expression language and task-orchestration runtime**, written in Scala 3 on [Cats Effect](https://typelevel.org/cats-effect/).

MiniCalc began as a small expression interpreter — a hand-written recursive
descent parser, an algebraic-data-type AST, and a pure, type-checked
evaluator — and grew into a miniature orchestration system: you can define
**tasks** with priorities, dependencies, timeouts, and retries, compose them
into **workflows**, and run them concurrently on Cats Effect fibers with a
dependency-aware scheduler and execution statistics — all from an interactive
REPL.

> **Stack:** Scala 3.3.6 · Cats Effect 3.5 · ScalaTest 3.2 · sbt

---

## Contents

- [Features](#features)
- [Getting started](#getting-started)
- [The expression language](#the-expression-language)
- [Tasks, scheduling & workflows](#tasks-scheduling--workflows)
- [REPL commands](#repl-commands)
- [Example session](#example-session)
- [Architecture](#architecture)
- [Project structure](#project-structure)
- [Testing](#testing)
- [Design principles](#design-principles)

---

## Features

### Expression language
- **Literals** — numbers (`42`, `3.14`) and booleans (`true`, `false`)
- **Variables** and **`let` bindings** — `let x = 10 in x * 2`
- **Arithmetic** `+ - * /`, **comparisons** `< > <= >= == !=`, **logic** `&& ||`
- **Conditionals** — `if x > 5 then 100 else 200`
- Correct **operator precedence & associativity**, all in a hand-written
  recursive descent parser (no parser generator)
- **Type-safe evaluation** with clear, position-aware error messages

### Task orchestration & concurrency
- **Task definitions** with optional **priority** (`High`/`Normal`/`Low`),
  **dependencies**, **timeouts**, and **retry policies** (linear or
  exponential backoff)
- **Composition** — run tasks in `sequence` or `parallel`
- **Dependency-aware scheduling** via a topological scheduler (Kahn's
  algorithm) with **circular-dependency detection** (three-color DFS) and
  unresolvable-dependency reporting
- **Concurrent execution** on **Cats Effect fibers** with a configurable
  maximum concurrency, per-task timeouts, and automatic retries
- **Workflows** — name a composition and run it with `:run`
- **Execution statistics** — started / completed / failed counts, average
  duration, and currently-running tasks

### Interactive REPL
- Full command set (`:help`, `:env`, `:tasks`, `:run`, `:stats`, `:set`, …)
- Tunable settings: `continue-on-error`, `verbose` (show value types),
  `show-timing` (show execution time)
- A built-in `:demo` that walks through every feature

---

## Getting started

### Prerequisites
- **Scala 3.3.6** (or compatible)
- **sbt 1.x**

### Run the REPL
```bash
sbt run
```
```text
Welcome to MiniCalc!
Type :help for help, :quit to exit
>
```

### Run the tests
```bash
sbt test

# A single suite
sbt "testOnly minicalc.EvaluatorSpec"

# Verbose output
sbt "testOnly * -- -oD"
```

---

## The expression language

```text
> 1 + 2 * 3
7.0

> let x = 5 in x * 2
10.0

> let x = 5 in let y = 10 in x + y
15.0

> if 10 > 5 then true else false
true

> (5 + 3) * 2 > 10 && true
true
```

Values print as `Double` or `Boolean`. A top-level `let` binding persists in
the REPL session (the environment is threaded through evaluation), so you can
reuse it on later lines and inspect it with `:env`.

### Operator precedence

From lowest to highest binding:

1. Logical OR — `||`
2. Logical AND — `&&`
3. Comparisons — `< > <= >= == !=`
4. Addition / subtraction — `+ -`
5. Multiplication / division — `* /`
6. Parentheses, literals, variables

### Error handling

Errors are returned as values (via `Either`), never thrown for control flow,
so the REPL reports them and keeps going:

```text
> x + 5
Error: Undefined variable: 'x'

> 10 / 0
Error: Cannot divide by zero: 10.0 / 0.0

> 5 + true
Error: Type mismatch: expected Number, got Boolean in 'true'

> if 5 then 10 else 20
Error: Type mismatch: expected Boolean, got Number in '5.0'
```

Parse errors include a caret pointing at the offending position.

---

## Tasks, scheduling & workflows

A **task** wraps a unit of work (`action: IO[A]`) with configuration:

```text
define task <name> = <expr>
    [with priority High|Normal|Low]      (optional)
    [depends on task1, task2, ...]       (optional)
    [timeout <seconds>]                  (optional)
    [retry <attempts> backoff <seconds>  (optional)
        exponential = true|false]        (optional, default false)
```

```text
# Define tasks (modifiers may appear in any order)
> define task fetchData = 42 with priority High timeout 30 retry 3 backoff 5
> define task processData = 100 depends on fetchData

# Inspect them
> :tasks
fetchData [Priority: High, Dependencies: none, Timeout: 30 seconds, ...]
processData [Priority: Normal, Dependencies: fetchData, Timeout: none, ...]

# Execute
> execute fetchData

# Compose
> sequence [execute fetchData, execute processData]
> parallel [execute t1, execute t2]

# Schedule with a strategy (FIFO keeps dependency order; PRIORITY sorts by priority)
> schedule parallel [execute highPrio, execute lowPrio] with PRIORITY

# Workflows
> define workflow pipeline = sequence [execute fetchData, execute processData]
> :run pipeline
```

Under the hood, the **scheduler** resolves dependencies via Kahn's algorithm
(rejecting cycles and unresolvable references), and the **fiber execution
engine** runs the resulting plan concurrently — honoring each task's timeout
and retry policy — and returns an execution report.

---

## REPL commands

| Command | Description |
|---|---|
| `:help` | Show the command/syntax help |
| `:help full` | Detailed help with examples |
| `:demo` | Run an interactive demonstration of all features |
| `:env` | List current variable bindings |
| `:clear` | Clear all definitions (variables, tasks, workflows) |
| `:quit`, `:q` | Exit MiniCalc |
| `:tasks` | Show all defined tasks with their configuration |
| `:clear-tasks` | Clear all task definitions |
| `:workflows` | Show all defined workflows |
| `:run <name>` | Execute a workflow by name |
| `:stats` | Show execution statistics |
| `:reset-stats` | Reset execution statistics |
| `:settings` | Show current REPL settings |
| `:set <name> <value>` | Change a setting (`true/false`, `on/off`, `1/0`) |

**Settings:** `continue-on-error` (keep going when a task fails),
`verbose` (print `value : Type`), `show-timing` (print execution duration).

---

## Example session

```text
> let x = 5 in x * 2
10.0

> if 10 > 5 then true else false
true

> define task fetchData = 42 with priority High timeout 30 retry 3 backoff 5
> define task processData = 100 depends on fetchData
> define workflow pipeline = sequence [execute fetchData, execute processData]
> :run pipeline
TaskList(2 tasks)

> :stats
Execution Statistics:
  Tasks Started:   2
  Tasks Completed: 2
  Tasks Failed:    0
  ...
```

---

## Architecture

```text
source ──tokenize──▶ List[Token] ──recursive descent──▶ Expr (AST)
                                                          │
                              pure  eval ◀────────────────┤  expressions
                              IO    evalIO ◀──────────────┘  tasks / workflows
                                       │
                         schedule (topological + strategy)
                                       │
                     FiberExecutionEngine (Cats Effect, parTraverseN)
                                       │
                                 ExecutionReport
```

| Package | Responsibility |
|---|---|
| `ast` | `Expr` AST (literals, `let`, `if`, task & workflow nodes), runtime `Value`s, `Operators` |
| `parser` | Two-phase **tokenizer + recursive descent `Parser`** (expressions, tasks, workflows, scheduling) |
| `eval` | `Evaluator` — pure `eval` for expressions and effectful `evalIO` for tasks; immutable `Environment`; `EvalError` types |
| `task` | `Task[A]` model, fluent `TaskBuilder`, `Priority`, `RetryPolicy` (`NoRetry`/`LinearRetry`/`ExponentialRetry`), `TaskId`, `TaskResult`, `TaskError` |
| `scheduler` | `Scheduler` trait, `TopologicalScheduler` (Kahn + cycle detection), `SchedulingStrategy` (`FIFO`/`Priority`), `SchedulingError` |
| `execution` | `ExecutionEngine` trait, `FiberExecutionEngine` (Cats Effect fibers), `ExecutionConfig`, `ExecutionReport` |
| `monitoring` | `TaskMonitor` trait and `InMemoryMonitor` (execution statistics) |
| `repl` | `REPL` (`IOApp`), `HelpText`, `Demo` |

**Key types**

```scala
// A task is an IO action plus scheduling/execution configuration
case class Task[A](
  id: TaskId,
  name: String,
  priority: Priority,
  dependencies: Set[TaskId],
  action: IO[A],
  timeout: Option[FiniteDuration],
  retryPolicy: RetryPolicy,
  metadata: Map[String, String]
)

// The evaluation environment is immutable and threaded through evaluation
case class Environment(
  bindings: Map[String, Value],
  tasks: Map[String, Task[Value]],
  workflows: Map[String, Expr],
  scheduler: Scheduler,
  monitor: TaskMonitor,
  executionConfig: ExecutionConfig,
  settings: REPLSettings,
  metadata: Map[String, String]
)
```

The evaluator keeps a **single generic `evalBinaryOp[T, R]`** for every binary
operator, and the parser keeps a **single generic `parseBinaryOp`** for every
precedence level — so adding an operator is a localized change.

---

## Project structure

```text
src/
├── main/scala/minicalc/
│   ├── Main.scala                 # IOApp entry point → REPL
│   ├── ast/                       # Expr, Value, Operators
│   ├── parser/                    # tokenizer + recursive descent parser
│   ├── eval/                      # Evaluator, Environment, EvalError
│   ├── task/                      # Task, TaskBuilder, Priority, RetryPolicy, …
│   ├── scheduler/                 # Scheduler, TopologicalScheduler, strategies
│   ├── execution/                 # ExecutionEngine, FiberExecutionEngine, …
│   ├── monitoring/                # TaskMonitor, InMemoryMonitor
│   └── repl/                      # REPL, HelpText, Demo
└── test/scala/minicalc/           # ScalaTest suites (see below)
```

---

## Testing

An extensive [ScalaTest](https://www.scalatest.org/) suite spans every layer,
including **property-based** parser tests:

```text
EvaluatorSpec            FiberExecutionEngineSpec   ParserSpec
EnvironmentSpec          InMemoryMonitorSpec        ParserPropertySpec
EvalErrorSpec            TaskSpec                   REPLSpec
ValueSpec                TopologicalSchedulerSpec   ReplSettingsSpec
                         IntegrationSpec
```

```bash
sbt test
```

---

## Design principles

- **Pure functional core** — expression evaluation is a pure function; all
  effects (IO, concurrency, timing) live in Cats Effect `IO`.
- **Algebraic data types** — sealed traits + case classes give an AST and
  value model the compiler can check for exhaustiveness.
- **Errors as values** — `Either[EvalError, _]` and domain error types instead
  of exceptions for control flow.
- **Immutable, threaded `Environment`** — bindings, tasks, and workflows are
  carried forward without mutation.
- **Separation of concerns** — parsing, evaluation, scheduling, execution, and
  monitoring are independent packages behind small interfaces.
- **Generic abstractions** — one function each for binary-operator parsing and
  evaluation keeps the language easy to extend.

---

## Author

**Christopher Garland** — [github.com/cgarland-dev](https://github.com/cgarland-dev)

Originally a Scala functional-programming course project, extended into a
task-orchestration runtime.

## License

Educational / personal project.
