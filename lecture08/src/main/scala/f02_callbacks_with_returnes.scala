object f02_callbacks_with_returnes extends App {

  def multiplyWithCallback(a: Int, b: Int)(callback: Int => Unit): Unit = {
    val result = a * b
    callback(result)
  }

  // 2 * 2 * 2 * 2
  multiplyWithCallback(2, 2) { r1 =>
    multiplyWithCallback(r1, 2) { r2 =>
      multiplyWithCallback(r2, 2) { r3 =>
        multiplyWithCallback(r3, 2) { r4 =>
          println(r4)
        }
      }
    }
  }
}
