import java.util.UUID

object ex3_io_flatMap extends App {
  val boilPotatoes: IO[String] = IO.delay {
    println("варим картошку...")
    "сваренная картошка"
  }

  val cutSausage: IO[String] = IO.delay {
    println("Режем колбасу...")
    "нарезанная колбаса"
  }

  val makeSalad: IO[String] = for {
    potatoes <- boilPotatoes
    _        <- IO.println("Очищаем картошку...")
    sausage  <- cutSausage
    _        <- IO.println("Смешиваем ингредиенты...")
  } yield s"Оливье из: $potatoes и $sausage"
}
