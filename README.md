# MiniCalc - Functional Expression Evaluator

A purely functional interpreter for a custom expression language, built with Scala 3. MiniCalc demonstrates advanced functional programming concepts including algebraic data types, immutable data structures, and type-safe evaluation.

## Features

### Language Constructs
- **Numeric Literals**: `42`, `3.14`, `-5`
- **Boolean Literals**: `true`, `false`
- **Variables**: `x`, `myVar`, `counter`
- **Arithmetic Operations**: `+`, `-`, `*`, `/`
- **Comparison Operations**: `<`, `>`, `<=`, `>=`, `==`, `!=`
- **Logical Operations**: `&&`, `||`
- **Let Bindings**: `let x = 10 in x * 2`
- **Conditionals**: `if x > 5 then 100 else 200`

### Example Expressions

```scala
// Simple arithmetic
5 + 3 * 2                              // → 11.0

// Variables with let bindings
let x = 10 in x + 5                    // → 15.0

// Nested let bindings
let x = 5 in let y = 10 in x + y       // → 15.0

// Conditionals
if 10 > 5 then 100 else 200            // → 100.0

// Complex expressions
let x = 10 in if x > 5 then x * 2 else x / 2  // → 20.0

// Comparisons and logic
(5 + 3) * 2 > 10 && true               // → true
```

## Project Structure

```
src/
├── main/
│   └── scala/
│       └── minicalc/
│           ├── Main.scala                    # Entry point
│           ├── ast/
│           │   ├── Expr.scala               # Expression AST
│           │   ├── Value.scala              # Runtime values
│           │   └── Operators.scala          # Binary operators
│           ├── eval/
│           │   ├── Evaluator.scala          # Expression evaluator
│           │   ├── Environment.scala        # Variable bindings
│           │   └── EvalError.scala          # Error types
│           ├── parser/
│           │   └── Parser.scala             # Tokenizer & parser
│           └── repl/
│               └── REPL.scala               # Interactive REPL
└── test/
    └── scala/
        └── minicalc/
            ├── EvaluatorSpec.scala
            ├── ParserSpec.scala
            ├── EnvironmentSpec.scala
            ├── REPLSpec.scala
            └── IntegrationSpec.scala
```

## Getting Started

### Prerequisites
- Scala 3.3.6 or higher
- sbt 1.x

### Running the REPL

```bash
# Compile and run
sbt run

# You'll see:
Welcome to MiniCalc!
Type :help for help, :quit to exit
> 
```

### Running Tests

```bash
# Run all tests
sbt test

# Run specific test suite
sbt "testOnly minicalc.EvaluatorSpec"
sbt "testOnly minicalc.ParserSpec"

# Run with detailed output
sbt "testOnly * -- -oD"
```

## REPL Commands

| Command | Description |
|---------|-------------|
| `:help` | Show help message |
| `:env` | List current variable bindings |
| `:clear` | Clear all variable bindings |
| `:quit` or `:q` | Exit the REPL |

## Architecture

### Design Principles

1. **Pure Functional Programming**
   - No mutable state
   - All functions are pure (except IO in REPL)
   - Immutable data structures throughout

2. **Algebraic Data Types**
   - Sealed traits for exhaustive pattern matching
   - Compiler-checked completeness

3. **Functional Error Handling**
   - `Either[Error, Value]` for operations that can fail
   - No exceptions for control flow
   - Composable error handling with for-comprehensions

4. **Generic Programming**
   - Type parameters for reusable abstractions
   - `evalBinaryOp[T, R]` handles all binary operations
   - `parseBinaryOp` handles all precedence levels

### Key Components

#### AST (Abstract Syntax Tree)
Defines the structure of MiniCalc expressions using sealed traits and case classes.

```scala
sealed trait Expr
case class NumLit(value: Double) extends Expr
case class BinOp(op: BinaryOp, left: Expr, right: Expr) extends Expr
// ... more expression types
```

#### Evaluator
Pure functional evaluator that recursively processes expressions:
- Pattern matching on expression types
- Environment threading for variable lookups
- Type checking with helpful error messages

#### Parser
Two-phase parsing approach:
1. **Tokenization**: String → List[Token]
2. **Parsing**: List[Token] → Expr

Handles operator precedence correctly using recursive descent parsing.

#### Environment
Immutable map from variable names to values:
- All operations return new Environment instances
- Supports variable shadowing (inner bindings override outer)
- Metadata field for future extensibility

## Operator Precedence

From highest to lowest:

1. Parentheses: `(...)`
2. Multiplication, Division: `*`, `/`
3. Addition, Subtraction: `+`, `-`
4. Comparisons: `<`, `>`, `<=`, `>=`, `==`, `!=`
5. Logical AND: `&&`
6. Logical OR: `||`

## Error Handling

MiniCalc provides clear, helpful error messages:

```scala
> x + 5
Error: Undefined variable: x.

> 10 / 0
Error: Cannot divide by zero: division by zero.

> 5 + true
Error: Type mismatch in expression true: Expected Number, got Boolean.

> if 5 then 10 else 20
Error: Type mismatch in expression 5.0: Expected Boolean, got Number.
```

## Testing

Comprehensive test suite with 530+ tests covering:
- ✅ All expression types
- ✅ All operators
- ✅ Error conditions
- ✅ Edge cases
- ✅ Integration tests
- ✅ Parser precedence
- ✅ REPL functionality

## Technical Highlights

### Generic Binary Operation Evaluation
```scala
def evalBinaryOp[T, R](
  left: Expr,
  right: Expr,
  env: Environment,
  extract: Value => Either[EvalError, T],
  op: (T, T) => Either[EvalError, R]
): Either[EvalError, R]
```

This single function handles:
- Arithmetic operations (Double → Double)
- Comparisons (Double → Boolean)
- Logical operations (Boolean → Boolean)

### Operator Precedence Parsing
```scala
def parseBinaryOp(
  tokens: List[Token],
  operators: Set[String],
  nextParser: List[Token] => Either[ParseError, (Expr, List[Token])],
  opMap: String => BinaryOp
): Either[ParseError, (Expr, List[Token])]
```

Single generic function handles all precedence levels with proper associativity.

## Extensibility

The design is prepared for future extensions:

1. **Additional Operators**
   - Add to `Operators.scala`
   - Update evaluator pattern match
   - Add to parser precedence chain

2. **New Value Types**
   - Extend `Value` trait
   - Add extraction methods
   - Update type checking

3. **Additional Expression Types**
   - Extend `Expr` trait
   - Add evaluation case
   - Add parsing support

4. **IO Operations** (for final project)
   - `Environment.metadata` field ready
   - Pure `processInput` separates logic from IO
   - `evalIO` wrapper prepared

## Learning Outcomes

This project demonstrates:
- ✅ Advanced functional programming in Scala
- ✅ Type-driven design with ADTs
- ✅ Generic programming with type parameters
- ✅ Recursive descent parsing
- ✅ Immutable data structures
- ✅ Functional error handling
- ✅ Pure functions vs. side effects
- ✅ Comprehensive testing

## Build Configuration

```scala
// build.sbt
name := "MiniCalc"
version := "0.1.0"
scalaVersion := "3.3.6"

libraryDependencies += "org.scalatest" %% "scalatest" % "3.2.17" % Test
```

## Author

**Chris**  
Scala Functional Programming Course - Midterm Project

## License

Educational project for learning functional programming concepts.

---

**Built with ❤️ and functional programming in Scala**