package minicalc.eval

/**
 * Error types for evaluation, parsing, and task-related failures.
 *
 * Uses sealed trait to ensure exhaustive error handling.
 * Each error type carries context to provide helpful error messages.
 */

/** Base trait for all evaluation and parsing errors */
sealed trait EvalError {

  /** Human-readable error message */
  def message: String
}

// ============================================
// ARITHMETIC ERRORS
// ============================================

/**
 * Error raised when attempting to divide by zero.
 *
 * @param expr String representation of the expression that caused the error
 */
case class DivisionByZero(expr: String) extends EvalError {
  override def message: String = s"Cannot divide by zero: $expr"
}

// ============================================
// VARIABLE ERRORS
// ============================================

/**
 * Error raised when referencing an undefined variable.
 *
 * @param varName The name of the undefined variable
 */
case class UndefinedVariable(varName: String) extends EvalError {
  override def message: String = s"Undefined variable: '$varName'"
}

// ============================================
// TYPE ERRORS
// ============================================

/**
 * Error raised when an operation receives the wrong type of value.
 *
 * Example: Trying to add a number and a boolean (5 + true)
 *
 * @param expected The expected type (e.g., "Number")
 * @param got      The actual type received (e.g., "Boolean")
 * @param expr     String representation of the expression
 */
case class TypeMismatch(expected: String, got: String, expr: String) extends EvalError {
  override def message: String = {
    s"Type mismatch: expected $expected, got $got in '$expr'"
  }
}

// ============================================
// PARSE ERRORS
// ============================================

/**
 * Error raised during parsing when input syntax is invalid.
 *
 * @param reason   Description of what went wrong
 * @param position Optional character position where the error occurred
 */
case class ParseError(reason: String, position: Option[Int] = None) extends EvalError {
  override def message: String = {
    position match {
      case Some(pos) => s"Parse error at position $pos: $reason"
      case None      => s"Parse error: $reason"
    }
  }

  /**
   * Creates an enhanced error message showing context from the input.
   *
   * Displays a snippet of the input around the error position with
   * a caret (^) pointing to the exact location.
   *
   * @param input The original input string
   * @return Formatted error message with visual context
   */
  def messageWithContext(input: String): String = {
    position match {
      case Some(pos) =>
        // Find the line containing the error
        val lines = input.split("\n", -1)  // -1 to keep trailing empty strings
        var currentPos = 0
        var lineNum = 0
        var columnNum = pos

        // Find which line the position is on
        while (lineNum < lines.length && currentPos + lines(lineNum).length < pos) {
          currentPos += lines(lineNum).length + 1  // +1 for newline
          lineNum += 1
        }

        // Calculate column within that line
        if (lineNum < lines.length) {
          columnNum = pos - currentPos
        }

        val errorLine = if (lineNum < lines.length) lines(lineNum) else input
        val pointer = " " * columnNum + "^"

        s"""Parse error at position $pos (line ${lineNum + 1}, column ${columnNum + 1}): $reason
           |  $errorLine
           |  $pointer""".stripMargin

      case None =>
        s"Parse error: $reason"
    }
  }
}

// ============================================
// TASK ERRORS
// ============================================

/**
 * Error raised when trying to execute a task that doesn't exist.
 *
 * @param taskName The name of the task that wasn't found
 */
case class TaskNotFound(taskName: String) extends EvalError {
  override def message: String = s"Task not found: '$taskName'"
}

/**
 * Error raised when trying to run a workflow that doesn't exist.
 *
 * @param workflowName The name of the workflow that wasn't found
 */
case class WorkflowNotFound(workflowName: String) extends EvalError {
  override def message: String = s"Workflow not found: '$workflowName'"
}

/**
 * Error raised when a task execution fails.
 *
 * @param taskName The name of the task that failed
 * @param reason   Description of why it failed
 */
case class TaskExecutionFailed(taskName: String, reason: String) extends EvalError {
  override def message: String = s"Task '$taskName' failed: $reason"
}

/**
 * Error raised when task dependencies cannot be resolved.
 *
 * @param taskName    The task with unresolvable dependencies
 * @param missingDeps The names of the missing dependencies
 */
case class UnresolvedDependencies(taskName: String, missingDeps: List[String]) extends EvalError {
  override def message: String = {
    s"Task '$taskName' has unresolved dependencies: ${missingDeps.mkString(", ")}"
  }
}

/**
 * Error raised when a circular dependency is detected in tasks.
 *
 * @param cycle The list of task names forming the cycle
 */
case class CircularDependencyDetected(cycle: List[String]) extends EvalError {
  override def message: String = {
    s"Circular dependency detected: ${cycle.mkString(" -> ")}"
  }
}

// ============================================
// SCHEDULING ERRORS
// ============================================

/**
 * Error raised when task scheduling fails.
 *
 * @param reason Description of why scheduling failed
 */
case class SchedulingFailed(reason: String) extends EvalError {
  override def message: String = s"Scheduling failed: $reason"
}

// ============================================
// INTERNAL ERRORS
// ============================================

/**
 * Error raised when a task expression is used with the pure eval function.
 *
 * Task expressions require IO and must use evalIO instead.
 *
 * @param exprType The type of expression that was incorrectly evaluated
 */
case class UnsupportedInPureEval(exprType: String) extends EvalError {
  override def message: String = {
    s"$exprType requires IO effects - use evalIO instead of eval"
  }
}

/**
 * Error raised for unexpected internal errors.
 *
 * Should rarely occur - indicates a bug in the interpreter.
 *
 * @param reason Description of the internal error
 */
case class InternalError(reason: String) extends EvalError {
  override def message: String = s"Internal error: $reason"
}