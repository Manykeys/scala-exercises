object ex1_io_create extends App {

  val mayonnaise: IO[String] = IO.pure("мазик")

  val boilPotatoes: IO[String] = IO.delay {
    println("варим картошку...")
    "сваренная картошка"
  }

  val cutSausage: IO[String] = IO.delay {
    println("Режем колбасу...")
    "нарезанная колбаса"
  }

}
