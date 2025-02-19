package exercises02

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers.convertToAnyShouldWrapper
import org.scalatestplus.scalacheck.ScalaCheckDrivenPropertyChecks

class FunctionsSpec extends AnyFlatSpec with ScalaCheckDrivenPropertyChecks {
  it should "curry" in new Functions {
    forAll { (f: (Int, Int) => Int, a: Int, b: Int) =>
      assert(curry(f)(a)(b) == f(a, b))
    }
  }

  it should "uncurry" in new Functions {
    forAll { (f: Int => Int => Int, a: Int, b: Int) =>
      assert(uncurry(f)(a, b) == f(a)(b))
    }
  }

  it should "andThen" in new Functions {

    forAll { (f: Int => Int, g: Int => Int, n: Int) =>
      andThen(f)(g)(n) shouldBe g(f(n))
    }

  }

  it should "compose" in new Functions {
    forAll { (f: Int => Int, g: Int => Int, n: Int) =>
      compose(f)(g)(n) shouldBe f(g(n))
    }
  }

  it should "const" in new Functions {
    forAll { (s: String, i: Int) =>
      const[String, Int](i)(s) shouldBe i
    }
  }

  it should "liftOption" in new Functions {
    forAll { (f: Int => Int, i: Int) =>
      liftOption(f)(i) shouldBe Some(f(i))
    }
  }

  it should "chain" in new Functions {
    val list: List[Int => Int] = List(
      (_ * 2),
      (_ + 5),
      (_ - 11),
      (_ % 5)
    )

    val chained: Int => Option[Int] = chain(list)

    chained(10) shouldBe Some(4)
    chained(13) shouldBe Some(0)
  }

  it should "chain empty" in new Functions {
    val list: List[Int => Int] = List.empty[Int => Int]

    val chained: Int => Option[Int] = chain(list)

    forAll { (i: Int) =>
      chained(i) shouldBe None
    }
  }

  it should "zip" in new Functions {
    forAll { (f1: Int => String, f2: Int => Double, i: Int) =>
      {
        zip(f1, f2)(i) shouldBe (f1(i), f2(i))
      }
    }
  }

  it should "unzip" in new Functions {
    forAll { (func: Int => (String, Double), i: Int) =>
      {
        val (f, g) = unzip(func)
        func(i)._1 shouldBe f(i)
        func(i)._2 shouldBe g(i)
      }
    }
  }

}
