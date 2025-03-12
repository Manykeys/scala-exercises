import examples.typeclasses.Monoid
import examples.typeclasses.MonoidInstances._
import examples.typeclasses.MonoidSyntax._

def combineAll[A: Monoid](list: List[A]): A = {
  list.foldLeft(Monoid[A].empty)(_ |+| _)
}

combineAll(List(1, 2, 3))
combineAll[Int](Nil)

combineAll(List("Hello", "my", "little", "pony"))
combineAll[String](Nil)

combineAll[Option[Int]](List(Some(1), Some(2), Some(3)))
combineAll[Option[Int]](Nil)

// Laws

// Associativity
((1 |+| 2) |+| 3) == (1 |+| (2 |+| 3))

// Left identity
 (Monoid[Int].empty |+| 42) == 42
(42 |+| Monoid[Int].empty) == 42
