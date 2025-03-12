import examples.typeclasses.{Foldable, Monoid}
import examples.typeclasses.FoldableSyntax._
import examples.typeclasses.FoldableInstances._
import examples.typeclasses.MonoidInstances._
import examples.typeclasses.MonoidSyntax.MonoidOps

Vector(1, 2, 3).foldLeft(0) { case (a, b) => a + b } // result: 6
List(1, 2, 3).foldLeft(0) { case (a, b) => a + b } // result: 6
Option(1).foldLeft(0) { case (a, b) => a + b } // result: 1

// Foldable

// List implementation
def combineAll[A: Monoid](as: List[A]): A = {
  as.foldLeft(Monoid[A].empty)(_ |+| _)
}

combineAll(List(1, 2, 3, 4, 5))

// combineAll(Vector(1, 2, 3, 4, 5))

// combineAll(Option(42))
