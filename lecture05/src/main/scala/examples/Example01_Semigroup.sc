import examples.typeclasses.Semigroup
import examples.typeclasses.SemigroupInstances._
import examples.typeclasses.SemigroupSyntax._

import scala.language.implicitConversions

Semigroup[Int].combine(1, 2)
// result: 3

1 |+| 2 |+| 3

"Hello" |+| " " |+| "world!"

List(1, 2) |+| List(3, 4) |+| Nil

// Laws

// Associativity
((1 |+| 2) |+| 3) == (1 |+| (2 |+| 3))

// TODO: ???
def combineAll[A: Semigroup](list: List[A]): A = {
  ???
}
