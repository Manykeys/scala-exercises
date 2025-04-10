object f01_callbacks extends App {
  def printWithCallback(s: String)(callback: () => Unit): Unit = {
    println(s)
    callback()
  }

  printWithCallback("Почистить картошку") { () =>
    printWithCallback("Сварить картошку") { () =>
      printWithCallback("Нарезать картошку") { () =>
        ()
      }
    }
  }
}
