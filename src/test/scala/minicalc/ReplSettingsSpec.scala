package minicalc.repl

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

/**
 * Tests for REPLSettings.
 */
class REPLSettingsSpec extends AnyFlatSpec with Matchers {

  "REPLSettings" should "have sensible defaults" in {
    val settings = REPLSettings.default
    settings.continueOnError shouldBe false
    settings.verbose shouldBe false
    settings.showTiming shouldBe false
  }

  it should "support copy for modification" in {
    val settings = REPLSettings.default.copy(verbose = true)
    settings.verbose shouldBe true
    settings.continueOnError shouldBe false // Unchanged
  }

  it should "display settings with show" in {
    val settings = REPLSettings(
      continueOnError = true,
      verbose = false,
      showTiming = true
    )
    val shown = settings.show

    shown should include("continue-on-error")
    shown should include("true")
    shown should include("verbose")
    shown should include("false")
    shown should include("show-timing")
  }

  it should "be immutable" in {
    val original = REPLSettings.default
    val modified = original.copy(verbose = true)

    original.verbose shouldBe false
    modified.verbose shouldBe true
  }
}