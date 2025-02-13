package exercises00

import exercises00.Application
import org.scalacheck.{Gen, Prop}
import org.scalatestplus.scalacheck.Checkers

class ApplicationTest extends org.scalatest.wordspec.AnyWordSpec with Checkers {

  "Resolver" should {
    "work with 1" in check {
      Prop.forAll(Gen.hexStr) {
        string => Application.hello(string) == s"Hello $string"
      }
    }
  }


}
