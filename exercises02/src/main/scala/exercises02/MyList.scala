package exercises02

import exercises02.MyList.Filter.{Preserve, Skip}

import scala.annotation.tailrec

sealed trait MyList[+A]

final case class Cons[A](head: A, tail: MyList[A]) extends MyList[A]

case object Nil extends MyList[Nothing]

object MyList {
  @tailrec
  def foldLeft[A, B](list: MyList[A])(base: B)(f: (B, A) => B): B = {
    list match {
      case Cons(head, tail) => foldLeft(tail)(f(base, head))(f)
      case Nil              => base
    }
  }

  def sum(list: MyList[Int]): Int = foldLeft(list)(0)(_ + _)

  def reverse[A](list: MyList[A]): MyList[A] = {
    list match {
      case Cons(head, tail) =>
        foldLeft(tail)(Cons(head, Nil))((acc, elem) => Cons(elem, acc))
      case _ => list
    }
  }

  def last[A](myList: MyList[A]): Option[A] = myList match {
    case Cons(head, tail) => foldLeft(tail)(Option(head))((_, elem) => Option(elem))
    case Nil              => None
  }

  def size[A](myList: MyList[A], acc: Int = 1): Int = myList match {
    case Cons(_, tail) => foldLeft(tail)(1)((count, _) => count + 1)
    case Nil           => 0
  }

  def max[A](myList: MyList[A], isBigger: (A, A) => Boolean): Option[A] =
    myList match {
      case Cons(head, tail) => Option(foldLeft(tail)(head)((acc, el) => if (isBigger(acc, el)) acc else el))
      case _                => None
    }

  def filter[A](myList: MyList[A], predicate: A => Filter.Filter): MyList[A] = {
    def checkPredicate(item: A)(implicit predicate: A => Filter.Filter): Boolean =
      predicate(item) match {
        case Skip     => false
        case Preserve => true
      }

    implicit val filterPredicate: A => Filter.Filter = predicate

    myList match {
      case Cons(head, tail) =>
        reverse(
          foldLeft[A, MyList[A]](Cons(head, tail))(Nil)((acc, el) => if (checkPredicate(el)) Cons(el, acc) else acc)
        )
      case Nil => Nil
    }
  }

  object Filter {
    sealed trait Filter

    case object Skip extends Filter

    case object Preserve extends Filter
  }
}
