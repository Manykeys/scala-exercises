import examples.typeclasses.Monad
import examples.typeclasses.MonadInstances._
import examples.typeclasses.MonadSyntax._


val a: String          = "123"
val fa: Option[String] = Some("123")

val f: String => Option[Int] = _.toIntOption
val g: Int => Option[Double] = _.toDouble.pure[Option]

// Left identity
Monad[Option].pure(a).flatMap(f) == f(a)

// Right identity
fa.flatMap(Monad[Option].pure) == fa

// Associativity
fa.flatMap(f).flatMap(g) == fa.flatMap(a => f(a).flatMap(g))

