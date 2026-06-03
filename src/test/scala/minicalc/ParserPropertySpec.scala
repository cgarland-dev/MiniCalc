package minicalc.parser

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

import minicalc.ast._

/**
 * Additional parser tests covering edge cases and properties.
 */
class ParserPropertySpec extends AnyFlatSpec with Matchers {

  // ============================================
  // NUMBER PARSING
  // ============================================

  "Parser" should "parse various integers" in {
    val integers = List(0, 1, 42, 100, 999, 1000, 123456)
    integers.foreach { n =>
      val result = Parser.parse(n.toString)
      result.isRight shouldBe true
      result.toOption.get shouldBe NumLit(n.toDouble)
    }
  }

  it should "parse various decimals" in {
    val decimals = List(0.5, 1.0, 3.14, 99.99, 123.456)
    decimals.foreach { n =>
      val formatted = f"$n%.2f"
      val result = Parser.parse(formatted)
      result.isRight shouldBe true
    }
  }

  // ============================================
  // ARITHMETIC WITH VARIOUS NUMBERS
  // ============================================

  it should "parse addition with various numbers" in {
    val pairs = List((1, 2), (0, 0), (100, 200), (5, 10))
    pairs.foreach { case (a, b) =>
      val result = Parser.parse(s"$a + $b")
      result.isRight shouldBe true
      result.toOption.get shouldBe BinOp(Add, NumLit(a.toDouble), NumLit(b.toDouble))
    }
  }

  it should "parse subtraction with various numbers" in {
    val pairs = List((5, 3), (10, 0), (100, 50))
    pairs.foreach { case (a, b) =>
      val result = Parser.parse(s"$a - $b")
      result.isRight shouldBe true
    }
  }

  it should "parse multiplication with various numbers" in {
    val pairs = List((2, 3), (0, 100), (10, 10))
    pairs.foreach { case (a, b) =>
      val result = Parser.parse(s"$a * $b")
      result.isRight shouldBe true
    }
  }

  it should "parse division with non-zero divisors" in {
    val pairs = List((10, 2), (100, 5), (9, 3))
    pairs.foreach { case (a, b) =>
      val result = Parser.parse(s"$a / $b")
      result.isRight shouldBe true
    }
  }

  // ============================================
  // PRECEDENCE TESTS
  // ============================================

  it should "maintain precedence: * before +" in {
    val triples = List((1, 2, 3), (5, 10, 2), (0, 1, 1))
    triples.foreach { case (a, b, c) =>
      val result = Parser.parse(s"$a + $b * $c")
      result.isRight shouldBe true
      // Should be a + (b * c), not (a + b) * c
      result.toOption.get match {
        case BinOp(Add, NumLit(_), BinOp(Mul, _, _)) => succeed
        case _ => fail(s"Precedence not maintained for $a + $b * $c")
      }
    }
  }

  it should "maintain precedence: / before -" in {
    val result = Parser.parse("10 - 6 / 2")
    result.isRight shouldBe true
    result.toOption.get match {
      case BinOp(Sub, _, BinOp(Div, _, _)) => succeed
      case _ => fail("Precedence not maintained")
    }
  }

  it should "maintain precedence: && before ||" in {
    Parser.parse("true || false && true").toOption.get match {
      case BinOp(Or, _, BinOp(And, _, _)) => succeed
      case _ => fail("Precedence not maintained")
    }
  }

  // ============================================
  // ASSOCIATIVITY TESTS
  // ============================================

  it should "be left-associative for subtraction" in {
    // a - b - c should be (a - b) - c
    Parser.parse("1 - 2 - 3").toOption.get match {
      case BinOp(Sub, BinOp(Sub, _, _), _) => succeed
      case _ => fail("Not left-associative")
    }
  }

  it should "be left-associative for division" in {
    // a / b / c should be (a / b) / c
    Parser.parse("8 / 4 / 2").toOption.get match {
      case BinOp(Div, BinOp(Div, _, _), _) => succeed
      case _ => fail("Not left-associative")
    }
  }

  it should "be left-associative for addition" in {
    Parser.parse("1 + 2 + 3").toOption.get match {
      case BinOp(Add, BinOp(Add, _, _), _) => succeed
      case _ => fail("Not left-associative")
    }
  }

  it should "be left-associative for multiplication" in {
    Parser.parse("2 * 3 * 4").toOption.get match {
      case BinOp(Mul, BinOp(Mul, _, _), _) => succeed
      case _ => fail("Not left-associative")
    }
  }

  // ============================================
  // PARENTHESES TESTS
  // ============================================

  it should "respect parentheses for grouping" in {
    val triples = List((1, 2, 3), (5, 10, 2))
    triples.foreach { case (a, b, c) =>
      // (a + b) * c should group addition first
      val result = Parser.parse(s"($a + $b) * $c")
      result.isRight shouldBe true
      result.toOption.get match {
        case BinOp(Mul, BinOp(Add, _, _), _) => succeed
        case _ => fail(s"Parentheses not respected for ($a + $b) * $c")
      }
    }
  }

  it should "handle nested parentheses" in {
    val result = Parser.parse("((1 + 2) * 3)")
    result.isRight shouldBe true
    result.toOption.get match {
      case BinOp(Mul, BinOp(Add, _, _), _) => succeed
      case _ => fail("Nested parentheses not handled")
    }
  }

  // ============================================
  // ERROR HANDLING TESTS
  // ============================================

  it should "reject invalid characters" in {
    val invalidExprs = List("1 @ 2", "1 # 2", "1 $ 2", "1 % 2", "1 ^ 2", "1 ~ 2")
    invalidExprs.foreach { expr =>
      val result = Parser.parse(expr)
      result.isLeft shouldBe true
    }
  }

  it should "reject incomplete expressions" in {
    val incomplete = List("1 +", "+ 2", "let x =", "if true then", "if true then 1 else")
    incomplete.foreach { expr =>
      val result = Parser.parse(expr)
      result.isLeft shouldBe true
    }
  }

  it should "reject unbalanced parentheses" in {
    val unbalanced = List("(1 + 2", "1 + 2)", "((1 + 2)", "(1 + (2)")
    unbalanced.foreach { expr =>
      val result = Parser.parse(expr)
      result.isLeft shouldBe true
    }
  }

  // ============================================
  // WHITESPACE HANDLING TESTS
  // ============================================

  it should "parse expressions regardless of whitespace" in {
    val pairs = List((1, 2), (10, 20), (5, 5))
    pairs.foreach { case (a, b) =>
      val compact = Parser.parse(s"$a+$b")
      val spaced = Parser.parse(s"$a + $b")
      val extraSpaced = Parser.parse(s"  $a  +  $b  ")

      compact shouldBe spaced
      spaced shouldBe extraSpaced
    }
  }

  it should "handle tabs and newlines as whitespace" in {
    val result1 = Parser.parse("1\t+\t2")
    val result2 = Parser.parse("1\n+\n2")
    val result3 = Parser.parse("1 + 2")

    result1 shouldBe result3
    result2 shouldBe result3
  }

  // ============================================
  // COMPLEX EXPRESSIONS
  // ============================================

  it should "parse complex arithmetic expressions" in {
    val result = Parser.parse("1 + 2 * 3 - 4 / 2")
    result.isRight shouldBe true
  }

  it should "parse complex boolean expressions" in {
    val result = Parser.parse("true && false || true && true")
    result.isRight shouldBe true
  }

  it should "parse mixed comparison and logical expressions" in {
    val result = Parser.parse("1 < 2 && 3 > 2 || 4 == 4")
    result.isRight shouldBe true
  }

  // ============================================
  // VALID VARIABLE NAMES
  // ============================================

  it should "parse various valid variable names" in {
    val validNames = List("x", "y", "foo", "bar", "myVar", "my_var", "var1", "x1y2z3")
    validNames.foreach { name =>
      val result = Parser.parse(name)
      result.isRight shouldBe true
      result.toOption.get shouldBe Var(name)
    }
  }
}