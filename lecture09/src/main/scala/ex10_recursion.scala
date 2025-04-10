object ex10_recursion extends App {
  def cutCucumbersBad(count: Int): IO[Unit] =
    if (count <= 0) IO.unit
    else IO(println(s"Нарезан огурец №${count}")).map(_ => ()).flatMap(_ => cutCucumbersBad(count - 1))

  def cutCucumbersGood(count: Int): IO[Unit] =
    if (count <= 0) IO.unit
    else IO(println(s"Нарезан огурец №${count}")).flatMap(_ => cutCucumbersGood(count - 1))

  def cutCucumbersTailRec(count: Int, acc: IO[Unit] = IO.unit): IO[Unit] =
    if (count <= 0) acc
    else cutCucumbersTailRec(count - 1, acc.flatMap(_ => IO(println(s"Нарезан огурец №${count}"))))

}
