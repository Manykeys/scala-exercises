object ex2_io_map extends App {

  val boilPotatoes: IO[String] = IO.delay {
    println("варим картошку...")
    "сваренная картошка"
  }

  val peeledPotatoes: IO[String] = boilPotatoes.map(p => s"очищенная $p")
}
