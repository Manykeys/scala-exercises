import examples.data.State
import examples.data.State._
import examples.typeclasses.MonadSyntax._

def updateStateExample(str: String): State[Int, String] =
  for {
    current <- State.ask[Int, Int](identity)
    _ <- str match {
      case "increment" => State.update[Int](_ + 1)
      case "decrement" => State.update[Int](_ - 1)
      case _           => State.update[Int](identity) // do nothing
    }
    updated <- State.ask[Int, Int](identity)
  } yield if (current != updated) "updated" else "unchanged"

updateStateExample("increment").run(100)

updateStateExample("decrement").run(100)

updateStateExample("???").run(100)
