package minicalc.parser

import minicalc.ast._
import minicalc.eval.ParseError
import minicalc.scheduler.{FIFOStrategy, PriorityStrategy}

/**
 * Recursive descent parser for MiniCalc expressions.
 *
 * Parsing happens in two phases:
 * 1. Tokenization: String → List[Token]
 * 2. Parsing: List[Token] → Expr
 *
 * The parser tracks character positions throughout to provide
 * meaningful error messages with location information.
 *
 * The parser handles operator precedence correctly (lowest to highest):
 * - || (logical OR)
 * - && (logical AND)
 * - <, >, <=, >=, ==, != (comparison)
 * - +, - (addition/subtraction)
 * - *, / (multiplication/division)
 * - Parentheses, literals, variables (primary)
 *
 * All internal types and functions are private - only parse() is exposed.
 */
object Parser {

  // ============================================
  // TOKEN TYPES (Internal representation)
  // ============================================

  /**
   * Base trait for all tokens produced by the tokenizer.
   *
   * Each token tracks its position in the original input string
   * for error reporting.
   *
   * @param position Character position where this token starts (0-indexed)
   */
  private sealed trait Token {
    def position: Int
  }

  /** Numeric literal token (e.g., 42, 3.14) */
  private case class NumberToken(value: Double, position: Int) extends Token

  /** Boolean literal token (true or false) */
  private case class BoolToken(value: Boolean, position: Int) extends Token

  /** Variable or identifier token */
  private case class VarToken(name: String, position: Int) extends Token

  /** Operator token (+, -, *, /, etc.) */
  private case class OpToken(op: String, position: Int) extends Token

  /** Keyword token (let, if, define, etc.) */
  private case class KeywordToken(keyword: String, position: Int) extends Token

  /** Left parenthesis '(' */
  private case class LParen(position: Int) extends Token

  /** Right parenthesis ')' */
  private case class RParen(position: Int) extends Token

  /** Left bracket '[' */
  private case class LBracket(position: Int) extends Token

  /** Right bracket ']' */
  private case class RBracket(position: Int) extends Token

  /** Comma separator ',' */
  private case class Comma(position: Int) extends Token

  /** Assignment operator '=' */
  private case class Assign(position: Int) extends Token

  // ============================================
  // HELPER DATA STRUCTURES
  // ============================================

  /**
   * Container for optional task definition modifiers.
   *
   * Accumulates the optional parts of a task definition as they are parsed.
   *
   * @param priority     Optional priority expression (High, Normal, Low)
   * @param dependencies List of task names this task depends on
   * @param timeout      Optional timeout expression in seconds
   * @param retryConfig  Optional retry configuration
   */
  private case class TaskModifiers(
      priority: Option[Expr],
      dependencies: List[String],
      timeout: Option[Expr],
      retryConfig: Option[RetryConfig]
  )

  // ============================================
  // POSITION TRACKING HELPERS
  // ============================================

  /**
   * Gets the position from the first token in a list, or 0 if empty.
   *
   * @param tokens Token list
   * @return Position of first token, or 0
   */
  private def positionOf(tokens: List[Token]): Int = {
    tokens.headOption.map(_.position).getOrElse(0)
  }

  /**
   * Creates a ParseError with position from the token list.
   *
   * @param message Error message
   * @param tokens  Current token list (used for position)
   * @return ParseError with position information
   */
  private def parseError(message: String, tokens: List[Token]): ParseError = {
    ParseError(message, Some(positionOf(tokens)))
  }

  /**
   * Creates a ParseError with a specific position.
   *
   * @param message  Error message
   * @param position Character position
   * @return ParseError with position information
   */
  private def parseErrorAt(message: String, position: Int): ParseError = {
    ParseError(message, Some(position))
  }

  // ============================================
  // TOKENIZATION
  // ============================================

  /**
   * Consumes characters while a predicate is true.
   *
   * Used for parsing multi-character tokens like numbers and identifiers.
   *
   * @param chars     Character list to consume from
   * @param predicate Condition to keep consuming
   * @return Tuple of (consumed string, remaining chars)
   */
  private def takeWhile(
      chars: List[Char],
      predicate: Char => Boolean
  ): (String, List[Char]) = {
    val (taken, rest) = chars.span(predicate)
    (taken.mkString, rest)
  }

  /**
   * Converts input string into a list of tokens.
   *
   * Handles:
   * - Whitespace (ignored)
   * - Numbers (integers and decimals)
   * - Identifiers and keywords
   * - Operators (single and double-character)
   * - Parentheses, brackets, and assignment
   *
   * Each token includes its position in the original input.
   *
   * @param input The source code string
   * @return Either a parse error or list of tokens
   */
  private def tokenize(input: String): Either[ParseError, List[Token]] = {
    def loop(
        chars: List[Char],
        position: Int,
        acc: List[Token]
    ): Either[ParseError, List[Token]] = {
      chars match {
        // End of input - return accumulated tokens in correct order
        case Nil => Right(acc.reverse)

        // Skip whitespace (but track position)
        case c :: rest if c.isWhitespace =>
          loop(rest, position + 1, acc)

        // Parse numbers (supports decimals like 3.14)
        case c :: rest if c.isDigit =>
          val (numStr, remaining) = takeWhile(c :: rest, ch => ch.isDigit || ch == '.')
          try {
            val num = numStr.toDouble
            loop(remaining, position + numStr.length, NumberToken(num, position) :: acc)
          } catch {
            case _: NumberFormatException =>
              Left(parseErrorAt(s"Invalid number: $numStr", position))
          }

        // Parse keywords and identifiers
        case c :: rest if c.isLetter =>
          val (word, remaining) = takeWhile(c :: rest, ch => ch.isLetterOrDigit || ch == '_')
          val token = word match {
            // Boolean literals
            case "true"  => BoolToken(true, position)
            case "false" => BoolToken(false, position)

            // Control flow keywords
            case "let"  => KeywordToken("let", position)
            case "in"   => KeywordToken("in", position)
            case "if"   => KeywordToken("if", position)
            case "then" => KeywordToken("then", position)
            case "else" => KeywordToken("else", position)

            // Task definition keywords
            case "task"     => KeywordToken("task", position)
            case "define"   => KeywordToken("define", position)
            case "execute"  => KeywordToken("execute", position)
            case "workflow" => KeywordToken("workflow", position)

            // Task execution keywords
            case "sequence" => KeywordToken("sequence", position)
            case "parallel" => KeywordToken("parallel", position)
            case "schedule" => KeywordToken("schedule", position)

            // Task modifier keywords
            case "with"        => KeywordToken("with", position)
            case "priority"    => KeywordToken("priority", position)
            case "depends"     => KeywordToken("depends", position)
            case "on"          => KeywordToken("on", position)
            case "timeout"     => KeywordToken("timeout", position)
            case "retry"       => KeywordToken("retry", position)
            case "backoff"     => KeywordToken("backoff", position)
            case "exponential" => KeywordToken("exponential", position)

            // Scheduling strategy keywords
            case "FIFO"     => KeywordToken("FIFO", position)
            case "PRIORITY" => KeywordToken("PRIORITY", position)

            // Not a keyword - treat as variable name
            case _ => VarToken(word, position)
          }
          loop(remaining, position + word.length, token :: acc)

        // Parse parentheses and brackets
        case '(' :: rest => loop(rest, position + 1, LParen(position) :: acc)
        case ')' :: rest => loop(rest, position + 1, RParen(position) :: acc)
        case '[' :: rest => loop(rest, position + 1, LBracket(position) :: acc)
        case ']' :: rest => loop(rest, position + 1, RBracket(position) :: acc)
        case ',' :: rest => loop(rest, position + 1, Comma(position) :: acc)

        // Parse operators (check two-char operators FIRST to avoid partial matches)
        case '<' :: '=' :: rest => loop(rest, position + 2, OpToken("<=", position) :: acc)
        case '>' :: '=' :: rest => loop(rest, position + 2, OpToken(">=", position) :: acc)
        case '!' :: '=' :: rest => loop(rest, position + 2, OpToken("!=", position) :: acc)
        case '=' :: '=' :: rest => loop(rest, position + 2, OpToken("==", position) :: acc)
        case '&' :: '&' :: rest => loop(rest, position + 2, OpToken("&&", position) :: acc)
        case '|' :: '|' :: rest => loop(rest, position + 2, OpToken("||", position) :: acc)

        // Single-character operators
        case '+' :: rest => loop(rest, position + 1, OpToken("+", position) :: acc)
        case '-' :: rest => loop(rest, position + 1, OpToken("-", position) :: acc)
        case '*' :: rest => loop(rest, position + 1, OpToken("*", position) :: acc)
        case '/' :: rest => loop(rest, position + 1, OpToken("/", position) :: acc)
        case '<' :: rest => loop(rest, position + 1, OpToken("<", position) :: acc)
        case '>' :: rest => loop(rest, position + 1, OpToken(">", position) :: acc)
        case '=' :: rest => loop(rest, position + 1, Assign(position) :: acc)

        // Unknown character
        case c :: _ =>
          Left(parseErrorAt(s"Unexpected character: '$c'", position))
      }
    }

    loop(input.toList, 0, List.empty)
  }

  // ============================================
  // EXPRESSION PARSING - Operator Precedence
  // ============================================

  /**
   * Generic binary operator parser with precedence handling.
   *
   * Implements left-to-right associativity by recursively parsing
   * operators at the same precedence level.
   *
   * @param tokens     Current token list
   * @param operators  Set of operator strings at this precedence level
   * @param nextParser Parser for next higher precedence level
   * @param opMap      Function to convert string to BinaryOp
   * @return Either error or (parsed expression, remaining tokens)
   */
  private def parseBinaryOp(
      tokens: List[Token],
      operators: Set[String],
      nextParser: List[Token] => Either[ParseError, (Expr, List[Token])],
      opMap: String => BinaryOp
  ): Either[ParseError, (Expr, List[Token])] = {

    // Recursively parse operators at same precedence level (left-to-right)
    def parseRest(
        left: Expr,
        tokens: List[Token]
    ): Either[ParseError, (Expr, List[Token])] = {
      tokens match {
        case OpToken(op, _) :: rest if operators.contains(op) =>
          nextParser(rest).flatMap { case (right, afterRight) =>
            // Build left-associative tree and continue
            parseRest(BinOp(opMap(op), left, right), afterRight)
          }
        case _ =>
          // No more operators at this level
          Right((left, tokens))
      }
    }

    // Parse left operand at higher precedence, then check for operators
    nextParser(tokens).flatMap { case (left, rest) =>
      parseRest(left, rest)
    }
  }

  /**
   * Entry point for expression parsing.
   * Starts with lowest precedence (OR) and works up.
   */
  private def parseExpr(
      tokens: List[Token]
  ): Either[ParseError, (Expr, List[Token])] = {
    parseOr(tokens)
  }

  /**
   * Parses OR operations (lowest precedence).
   */
  private def parseOr(
      tokens: List[Token]
  ): Either[ParseError, (Expr, List[Token])] = {
    parseBinaryOp(tokens, Set("||"), parseAnd, Op.stringToOp)
  }

  /**
   * Parses AND operations.
   */
  private def parseAnd(
      tokens: List[Token]
  ): Either[ParseError, (Expr, List[Token])] = {
    parseBinaryOp(tokens, Set("&&"), parseComparison, Op.stringToOp)
  }

  /**
   * Parses comparison operations (<, >, <=, >=, ==, !=).
   */
  private def parseComparison(
      tokens: List[Token]
  ): Either[ParseError, (Expr, List[Token])] = {
    parseBinaryOp(
      tokens,
      Set("<", ">", "<=", ">=", "==", "!="),
      parseAddSub,
      Op.stringToOp
    )
  }

  /**
   * Parses addition and subtraction operations.
   */
  private def parseAddSub(
      tokens: List[Token]
  ): Either[ParseError, (Expr, List[Token])] = {
    parseBinaryOp(tokens, Set("+", "-"), parseMulDiv, Op.stringToOp)
  }

  /**
   * Parses multiplication and division operations (highest binary precedence).
   */
  private def parseMulDiv(
      tokens: List[Token]
  ): Either[ParseError, (Expr, List[Token])] = {
    parseBinaryOp(tokens, Set("*", "/"), parsePrimary, Op.stringToOp)
  }

  // ============================================
  // LIST PARSING HELPERS
  // ============================================

  /**
   * Parses a comma-separated list of expressions within brackets.
   *
   * Expects the opening '[' to already be consumed. Parses until ']'.
   *
   * @param tokens Tokens after the opening bracket
   * @return Either error or (list of expressions, remaining tokens)
   */
  private def parseExprList(
      tokens: List[Token]
  ): Either[ParseError, (List[Expr], List[Token])] = {
    def loop(
        acc: List[Expr],
        tokens: List[Token]
    ): Either[ParseError, (List[Expr], List[Token])] = {
      tokens match {
        // Empty list or end of list
        case RBracket(_) :: rest =>
          Right((acc, rest))

        // Parse next expression
        case _ =>
          parseExpr(tokens) match {
            case Left(err) => Left(err)
            case Right((expr, afterExpr)) =>
              afterExpr match {
                case Comma(_) :: rest =>
                  // More expressions follow
                  loop(acc :+ expr, rest)
                case RBracket(_) :: rest =>
                  // End of list
                  Right((acc :+ expr, rest))
                case _ =>
                  Left(parseError("Expected ',' or ']' in list", afterExpr))
              }
          }
      }
    }

    loop(List.empty, tokens)
  }

  /**
   * Parses a comma-separated list of names (for dependencies).
   *
   * Unlike parseExprList, this parses variable names only, not expressions,
   * and does not require brackets.
   *
   * @param tokens Current tokens
   * @return Either error or (list of names, remaining tokens)
   */
  private def parseNameList(
      tokens: List[Token]
  ): Either[ParseError, (List[String], List[Token])] = {
    def loop(
        acc: List[String],
        tokens: List[Token]
    ): Either[ParseError, (List[String], List[Token])] = {
      tokens match {
        case VarToken(name, _) :: rest =>
          rest match {
            case Comma(_) :: afterComma =>
              // More names follow
              loop(acc :+ name, afterComma)
            case _ =>
              // No comma - this is the last name
              Right((acc :+ name, rest))
          }
        case _ =>
          // No more names - return what we have
          Right((acc, tokens))
      }
    }

    loop(List.empty, tokens)
  }

  /**
   * Parses a task list expression (sequence or parallel).
   *
   * Factors out the common pattern for parsing sequence and parallel:
   * keyword [expr1, expr2, ...]
   *
   * @param tokens      Tokens after the keyword
   * @param keyword     The keyword that was matched (for error messages)
   * @param constructor Function to build the AST node from the expression list
   * @return Either error or (parsed expression, remaining tokens)
   */
  private def parseTaskList(
      tokens: List[Token],
      keyword: String,
      constructor: List[Expr] => Expr
  ): Either[ParseError, (Expr, List[Token])] = {
    tokens match {
      case LBracket(_) :: afterBracket =>
        parseExprList(afterBracket) match {
          case Right((expressions, remaining)) =>
            Right((constructor(expressions), remaining))
          case Left(err) =>
            Left(err)
        }
      case _ =>
        Left(parseError(s"Expected '[' after '$keyword'", tokens))
    }
  }

  // ============================================
  // TASK MODIFIER PARSING
  // ============================================

  /**
   * Parses optional modifiers for a task definition.
   *
   * Recursively parses modifiers in any order:
   * - with priority High|Normal|Low
   * - depends on name1, name2, ...
   * - timeout <number>
   * - retry <number> backoff <number> [exponential = true|false]
   *
   * @param tokens Current tokens
   * @param acc    Accumulated modifiers so far
   * @return Either error or (final modifiers, remaining tokens)
   */
  private def parseOptional(
      tokens: List[Token],
      acc: TaskModifiers
  ): Either[ParseError, (TaskModifiers, List[Token])] = {
    tokens match {
      // Priority modifier: with priority High|Normal|Low
      case KeywordToken("with", _) :: afterWith =>
        afterWith match {
          case KeywordToken("priority", _) :: afterPriority =>
            afterPriority match {
              case VarToken(p, _) :: rest if Set("High", "Normal", "Low").contains(p) =>
                parseOptional(rest, acc.copy(priority = Some(Var(p))))
              case _ =>
                Left(parseError("Expected 'High', 'Normal', or 'Low' after 'priority'", afterPriority))
            }
          case _ =>
            Left(parseError("Expected 'priority' after 'with'", afterWith))
        }

      // Dependencies modifier: depends on name1, name2, ...
      case KeywordToken("depends", _) :: afterDepends =>
        afterDepends match {
          case KeywordToken("on", _) :: afterOn =>
            parseNameList(afterOn) match {
              case Right((names, remaining)) =>
                parseOptional(remaining, acc.copy(dependencies = names))
              case Left(err) =>
                Left(err)
            }
          case _ =>
            Left(parseError("Expected 'on' after 'depends'", afterDepends))
        }

      // Timeout modifier: timeout <number>
      case KeywordToken("timeout", _) :: afterTimeout =>
        afterTimeout match {
          case NumberToken(n, _) :: rest =>
            parseOptional(rest, acc.copy(timeout = Some(NumLit(n))))
          case _ =>
            Left(parseError("Expected number after 'timeout'", afterTimeout))
        }

      // Retry modifier: retry <attempts> backoff <seconds> [exponential = true|false]
      case KeywordToken("retry", _) :: afterRetry =>
        afterRetry match {
          case NumberToken(retries, _) :: afterRetries =>
            afterRetries match {
              case KeywordToken("backoff", _) :: afterBackoff =>
                afterBackoff match {
                  case NumberToken(backoff, _) :: rest =>
                    rest match {
                      // Exponential backoff specified
                      case KeywordToken("exponential", _) :: Assign(_) :: BoolToken(exp, _) :: afterBool =>
                        val config = RetryConfig(
                          maxAttempts = retries.toInt,
                          backoffSeconds = backoff,
                          exponential = exp
                        )
                        parseOptional(afterBool, acc.copy(retryConfig = Some(config)))

                      // No exponential specified - default to false (linear)
                      case _ =>
                        val config = RetryConfig(
                          maxAttempts = retries.toInt,
                          backoffSeconds = backoff,
                          exponential = false
                        )
                        parseOptional(rest, acc.copy(retryConfig = Some(config)))
                    }
                  case _ =>
                    Left(parseError("Expected number after 'backoff'", afterBackoff))
                }
              case _ =>
                Left(parseError("Expected 'backoff' keyword after retry count", afterRetries))
            }
          case _ =>
            Left(parseError("Expected number after 'retry'", afterRetry))
        }

      // No more modifiers - return accumulated result
      case _ =>
        Right((acc, tokens))
    }
  }

  // ============================================
  // PRIMARY EXPRESSION PARSING
  // ============================================

  /**
   * Parses primary expressions (highest precedence).
   *
   * Handles:
   * - Literals (numbers, booleans)
   * - Variables
   * - Parenthesized expressions
   * - Let bindings
   * - If conditionals
   * - Task definitions and execution
   * - Workflow definitions
   * - Sequence/parallel/schedule expressions
   *
   * @param tokens Current token list
   * @return Either error or (parsed expression, remaining tokens)
   */
  private def parsePrimary(
      tokens: List[Token]
  ): Either[ParseError, (Expr, List[Token])] = {
    tokens match {
      // ============================================
      // LITERALS
      // ============================================

      case NumberToken(n, _) :: rest =>
        Right((NumLit(n), rest))

      case BoolToken(b, _) :: rest =>
        Right((BoolLit(b), rest))

      case VarToken(name, _) :: rest =>
        Right((Var(name), rest))

      // ============================================
      // PARENTHESIZED EXPRESSIONS
      // ============================================

      case LParen(pos) :: rest =>
        parseExpr(rest) match {
          case Right((expr, RParen(_) :: remaining)) =>
            Right((expr, remaining))
          case Right((_, remaining)) =>
            Left(parseError("Expected ')' to close parenthesis", remaining))
          case Left(err) =>
            Left(err)
        }

      // ============================================
      // LET BINDINGS: let <name> = <value> in <body>
      // ============================================

      case KeywordToken("let", _) :: rest =>
        rest match {
          case VarToken(name, _) :: afterVar =>
            afterVar match {
              case Assign(_) :: afterAssign =>
                parseExpr(afterAssign) match {
                  case Right((valueExpr, afterValue)) =>
                    afterValue match {
                      case KeywordToken("in", _) :: afterIn =>
                        parseExpr(afterIn).map { case (bodyExpr, afterBody) =>
                          (Let(name, valueExpr, bodyExpr), afterBody)
                        }
                      case _ =>
                        Left(parseError("Expected 'in' keyword after let binding value", afterValue))
                    }
                  case Left(err) =>
                    Left(err)
                }
              case _ =>
                Left(parseError("Expected '=' after variable name in let binding", afterVar))
            }
          case _ =>
            Left(parseError("Expected variable name after 'let'", rest))
        }

      // ============================================
      // CONDITIONALS: if <cond> then <then> else <else>
      // ============================================

      case KeywordToken("if", _) :: rest =>
        parseExpr(rest) match {
          case Right((condExpr, afterCond)) =>
            afterCond match {
              case KeywordToken("then", _) :: afterThen =>
                parseExpr(afterThen) match {
                  case Right((thenExpr, afterThenExpr)) =>
                    afterThenExpr match {
                      case KeywordToken("else", _) :: afterElse =>
                        parseExpr(afterElse).map { case (elseExpr, afterElseExpr) =>
                          (If(condExpr, thenExpr, elseExpr), afterElseExpr)
                        }
                      case _ =>
                        Left(parseError("Expected 'else' keyword in conditional", afterThenExpr))
                    }
                  case Left(err) =>
                    Left(err)
                }
              case _ =>
                Left(parseError("Expected 'then' keyword after condition", afterCond))
            }
          case Left(err) =>
            Left(err)
        }

      // ============================================
      // TASK EXECUTION: execute <taskName>
      // ============================================

      case KeywordToken("execute", _) :: rest =>
        rest match {
          case VarToken(taskName, _) :: remaining =>
            Right((TaskExec(taskName), remaining))
          case _ =>
            Left(parseError("Expected task name after 'execute'", rest))
        }

      // ============================================
      // DEFINITIONS: define task|workflow ...
      // ============================================

      case KeywordToken("define", _) :: rest =>
        rest match {
          // Task definition:
          //   define task <name> = <body> [modifiers...]
          case KeywordToken("task", _) :: afterTask =>
            afterTask match {
              case VarToken(taskName, _) :: afterTaskName =>
                afterTaskName match {
                  case Assign(_) :: afterAssign =>
                    parseExpr(afterAssign) match {
                      case Right((taskExpr, afterExpr)) =>
                        val emptyModifiers = TaskModifiers(None, List.empty, None, None)
                        parseOptional(afterExpr, emptyModifiers) match {
                          case Right((modifiers, remaining)) =>
                            val taskDef = TaskDef(
                              taskName,
                              taskExpr,
                              modifiers.priority,
                              modifiers.dependencies,
                              modifiers.timeout,
                              modifiers.retryConfig
                            )
                            Right((taskDef, remaining))
                          case Left(err) =>
                            Left(err)
                        }
                      case Left(err) =>
                        Left(err)
                    }
                  case _ =>
                    Left(parseError("Expected '=' after task name", afterTaskName))
                }
              case _ =>
                Left(parseError("Expected task name after 'task'", afterTask))
            }

          // Workflow definition:
          //   define workflow <name> = <body>
          case KeywordToken("workflow", _) :: afterWorkflow =>
            afterWorkflow match {
              case VarToken(workflowName, _) :: afterName =>
                afterName match {
                  case Assign(_) :: afterAssign =>
                    parseExpr(afterAssign) match {
                      case Right((workflowExpr, remaining)) =>
                        Right((WorkflowDef(workflowName, workflowExpr), remaining))
                      case Left(err) =>
                        Left(err)
                    }
                  case _ =>
                    Left(parseError("Expected '=' after workflow name", afterName))
                }
              case _ =>
                Left(parseError("Expected workflow name after 'workflow'", afterWorkflow))
            }

          case _ =>
            Left(parseError("Expected 'task' or 'workflow' after 'define'", rest))
        }

      // ============================================
      // TASK LISTS: sequence|parallel [...]
      // ============================================

      case KeywordToken("sequence", _) :: rest =>
        parseTaskList(rest, "sequence", TaskSequence.apply)

      case KeywordToken("parallel", _) :: rest =>
        parseTaskList(rest, "parallel", TaskParallel.apply)

      // ============================================
      // SCHEDULE: schedule <expr> with FIFO|PRIORITY
      // ============================================

      case KeywordToken("schedule", _) :: rest =>
        parseExpr(rest) match {
          case Right((taskExpr, afterTask)) =>
            afterTask match {
              case KeywordToken("with", _) :: afterWith =>
                afterWith match {
                  case KeywordToken("FIFO", _) :: afterStrategy =>
                    Right((ScheduleExpr(taskExpr, FIFOStrategy), afterStrategy))
                  case KeywordToken("PRIORITY", _) :: afterStrategy =>
                    Right((ScheduleExpr(taskExpr, PriorityStrategy), afterStrategy))
                  case _ =>
                    Left(parseError("Expected 'FIFO' or 'PRIORITY' after 'with'", afterWith))
                }
              case _ =>
                Left(parseError("Expected 'with' after schedule expression", afterTask))
            }
          case Left(err) =>
            Left(err)
        }

      // ============================================
      // FALLBACK - No valid expression found
      // ============================================

      case Nil =>
        Left(ParseError("Unexpected end of input", None))

      case _ =>
        Left(parseError("Expected expression", tokens))
    }
  }

  // ============================================
  // TOP-LEVEL PARSING
  // ============================================

  /**
   * Parses tokens into an expression, ensuring all tokens are consumed.
   *
   * @param tokens Token list to parse
   * @return Either error or parsed expression
   */
  private def parseTokens(tokens: List[Token]): Either[ParseError, Expr] = {
    parseExpr(tokens) match {
      case Right((expr, Nil)) =>
        Right(expr)
      case Right((_, remaining)) =>
        Left(parseError("Unexpected tokens after expression", remaining))
      case Left(err) =>
        Left(err)
    }
  }

  // ============================================
  // PUBLIC API
  // ============================================

  /**
   * Parses a MiniCalc expression from a string.
   *
   * This is the only public method - handles the complete pipeline:
   * String → Tokenize → Parse → Expr
   *
   * @param input Source code string
   * @return Either a parse error or the parsed expression AST
   */
  def parse(input: String): Either[ParseError, Expr] = {
    for {
      tokens <- tokenize(input)
      expr   <- parseTokens(tokens)
    } yield expr
  }
}