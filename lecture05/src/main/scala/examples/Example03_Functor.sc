import examples.typeclasses.Functor
import examples.typeclasses.FunctorInstances._
import examples.typeclasses.FunctorSyntax._


Functor[List].map(List(1, 2, 3))(_ + 1)
Functor[Option].map(Some(1))(_ + 1)

// ---

def checkMeaningOfLife(num: Int): String = {
  if (num == 42) s"$num is meaning of life"
  else s"$num isn't meaning of life"
}

def functorUsingExample[F[_]: Functor](numF: F[Int]): F[String] = {
  // Последовательность вычислений с map
  numF
    .map(checkMeaningOfLife)
    .map(_.toUpperCase)
    .map(_ + "!")
}

functorUsingExample[Option](Some(42)) // result: Some(42 IS MEANING OF LIFE)
functorUsingExample[Option](None) // result: None

functorUsingExample(List(1, 2, 42))
// result: List(
//   1 ISN'T MEANING OF LIFE!,
//   2 ISN'T MEANING OF LIFE!,
//   42 IS MEANING OF LIFE!
// )

functorUsingExample(Vector(1, 2, 42))
// result: Vector(
//   1 ISN'T MEANING OF LIFE!,
//   2 ISN'T MEANING OF LIFE!,
//   42 IS MEANING OF LIFE!
// )

// Laws

// Identity
def identity[A](a: A): A = a

val fa: List[Int]          = List(1, 2, 3)
Functor[List].map(fa)(x => x) == fa

// или
Functor[List].map(fa)(identity) == fa


// Composition
val f: Int => Long    = _.toLong
val g: Long => String = _.toString

fa.map(a => g(f(a))) == fa.map(f).map(g)
