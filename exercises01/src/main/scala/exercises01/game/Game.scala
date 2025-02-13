package exercises01.game

import scala.annotation.tailrec

class Game(controller: GameController) {
  /**
    * Игра угадай число
    * Ввод и вывод необходимо осуществлять с помощью методов controller
    *
    * Игра должна вызывать controller.askNumber перед каждой попыткой игрока угадать число
    * И вызвать controller.nextLine для получения ввода игрока
    * Если игрок ввел число меньше загаданного, игра должна вызвать controller.numberIsBigger
    * Если игрок ввел число больше загаданного, игра должна вызвать controller.numberIsSmaller
    * Если игрок угадал число, игра должна закончиться и вызвать controller.guessed
    * Если игрок написал GameController.IGiveUp, игра должна закончиться и вызвать controller.giveUp(number)
    * Если игрок ввел неизвестную комбинацию символов, надо вызвать contoller.wrongInput и продолжить игру
    *
    * @param number загаданное число
    */

  def play(number: Int): Unit = {
    def startGameLoop(input: String): Boolean = {
      input.toIntOption match {
        case Some(value) if value < number =>
          controller.numberIsBigger()
          true
        case Some(value) if value > number =>
          controller.numberIsSmaller()
          true
        case Some(_) =>
          controller.guessed()
          false
        case None =>
          controller.wrongInput()
          true
      }
    }

    @tailrec
    def startGame(): Unit = {
      controller.askNumber()
      val input = controller.nextLine()

      input match {
        case "I give up" => controller.giveUp(number)
        case _ =>
          val shouldRestartGame = startGameLoop(input)
          if (shouldRestartGame)
            startGame()
      }
    }

    startGame()
  }
}

