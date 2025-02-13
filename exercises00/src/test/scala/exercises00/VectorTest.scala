package exercises00

import exercises00.Vector.unapply
import org.scalacheck.{Gen, Prop}
import org.scalatest.Assertion
import org.scalatest.matchers.should.Matchers._
import org.scalatestplus.scalacheck.Checkers

import scala.collection.immutable
class VectorTest extends org.scalatest.wordspec.AnyWordSpec with Checkers {
  val vectorGen: Gen[Vector] =
    for {
      x <- Gen.double
      y <- Gen.double
      normalized <- Gen.posNum[Int]
    } yield new Vector(x * normalized, y * normalized)


  val a = new Vector(10.1, 20.0)
  val b = new Vector(20.3, 23.4)
  val c = new Vector(30.4, -40.156)

  val vectors: List[Vector] = a :: b :: c :: Nil

  val nonZeroErr: Double = 1e-3

  // Asserts that two vectors are equal by comparing their coordinates
  def assertEqualsNeatly(v1: Vector, v2: Vector, error: Double = 0): Boolean = {



    val xDif = Math.abs(v1.x - v2.x)
    val yDif = Math.abs(v1.y - v2.y)

    error >= xDif & error >= yDif
  }

  "Vector" should {
    "support plus operation" in check {
      Prop.forAll(vectorGen, vectorGen) {
        case (vec1, vec2) => assertEqualsNeatly(vec1 + vec2, new Vector(vec1.x + vec2.x, vec1.y + vec2.y))
      }
    }

    "support minus operation" in check {
      Prop.forAll(vectorGen, vectorGen) {
        case (vec1, vec2) =>
        assertEqualsNeatly(vec1 - vec2, new Vector(vec1.x - vec2.x, vec1.y - vec2.y))
      }
    }

    "support multiply on scalar operation" in check {
      Prop.forAll(vectorGen, Gen.double) {
        case (vec, scalar) => assertEqualsNeatly(vec * scalar, new Vector(vec.x * scalar, vec.y * scalar), nonZeroErr)
      }
    }

    "distributivity" in check {
      Prop.forAll(vectorGen, vectorGen, Gen.double) {
        case (vec1, vec2, scalar) =>
          assertEqualsNeatly(vec1 * scalar + vec2 * scalar, (vec1 + vec2) * scalar, nonZeroErr)
      }
    }

    "support unary minus operation" in check {
      Prop.forAll(vectorGen) {
        vec =>
          assertEqualsNeatly(-vec, new Vector(-vec.x, -vec.y))
      }
    }

    "support euclideanLength operation" in check {
      Prop.forAll(vectorGen) {
        vec =>
          assertEqualsNeatly(
           new Vector(vec.euclideanLength, 0),
           new Vector(Math.sqrt(vec.x * vec.x + vec.y * vec.y), 0),
           nonZeroErr
          )
      }
    }

    "support normalized operation" in new {
      Prop.forAll(vectorGen) {
        vec =>
        val length = Math.sqrt(vec.x * vec.x + vec.y * vec.y)
        (length == 0) || assertEqualsNeatly(
          vec.normalized,
          new Vector(vec.x / vec.euclideanLength, vec.y / vec.euclideanLength)
        )
      }

    }

    "support normalized zero operation" in {
      assertEqualsNeatly(
        new Vector(0, 0).normalized,
        new Vector(0, 0)
      )
    }

    "support toString operation" in check {
      Prop.forAll(vectorGen) {
        vec => vec.toString == s"Vector(${vec.x}, ${vec.y})"
      }
    }

    "support fromAngle operation" in check {

      Prop.forAll(Gen.double, Gen.posNum[Int]) {
        case (alpha, length) =>
          val alphaR = alpha * Math.PI * 2

          val vec = Vector.fromAngle(alphaR, length)
          val vecLength = Math.sqrt(vec.x * vec.x + vec.y * vec.y)
          val vecAngle = {
            val atan = Math.atan2(vec.y, vec.x)
            if (atan < 0) atan + 2 * Math.PI else atan
          }
          assertEqualsNeatly(
            new Vector(alphaR, length),
            new Vector(vecAngle, vecLength),
            nonZeroErr
          )
      }
    }

    "support sum operation" in check {
      Prop.forAll(Gen.listOfN(10, vectorGen)) {
        vectors =>
          val xS = vectors.map(_.x).sum
          val yS = vectors.map(_.y).sum
          val xyS = new Vector(xS, yS)
          assertEqualsNeatly(
           Vector.sum(vectors),
           xyS
          )
      }
    }

    "return zero sum on empty list" in {
      assertEqualsNeatly(Vector.sum(List.empty), new Vector(0, 0))
    }

    "support unapply operation" in check {
      Prop.forAll(vectorGen) {
       vec =>
        val Some((x, y)) = unapply(vec)
        x == vec.x && y == vec.y
      }
    }

    "support equals operation" in check {
      Prop.forAll(vectorGen) {
       vec => vec == vec
      }
    }

    "return false on equals with incompatible types" in check {
      Prop.forAll(vectorGen) {
       vec => vec != 1 && vec != "1" && vec != null && vec!= new Object() && vec!= false
      }
    }
  }
}
