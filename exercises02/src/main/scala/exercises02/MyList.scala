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
    @tailrec
    def loop(aList: MyList[A], acc: MyList[A]): MyList[A] = aList match {
      case Nil              => acc
      case Cons(head, tail) => loop(tail, Cons(head, acc))
    }

    loop(list, Nil)
  }

  @tailrec
  def last[A](myList: MyList[A]): Option[A] = myList match {
    case Cons(head, Nil) => Some(head)
    case Cons(_, tail)   => last(tail)
    case Nil             => None
  }

  @tailrec
  def size[A](myList: MyList[A], acc: Int = 1): Int = myList match {
    case Cons(_, Nil)  => acc
    case Cons(_, tail) => size(tail, acc + 1)
    case Nil           => 0
  }

  def max[A](myList: MyList[A], isBigger: (A, A) => Boolean): Option[A] = {
    @tailrec
    def maxAcc(tail: MyList[A], acc: A): Option[A] = {
      tail match {
        case Cons(head, Nil) if isBigger(head, acc)   => Some(head)
        case Cons(head, Nil) if !isBigger(head, acc)  => Some(acc)
        case Cons(head, tail) if isBigger(head, acc)  => maxAcc(tail, acc = head)
        case Cons(head, tail) if !isBigger(head, acc) => maxAcc(tail, acc = acc)
        case _                                        => None
      }
    }

    myList match {
      case Cons(head, Nil)  => Some(head)
      case Cons(head, tail) => maxAcc(tail, acc = head)
      case Nil              => None
    }
  }

  def filter[A](myList: MyList[A], predicate: A => Filter.Filter): MyList[A] = {
    def checkPredicate(item: A)(implicit predicate: A => Filter.Filter): Boolean =
      predicate(item) match {
        case Skip     => false
        case Preserve => true
      }

    @tailrec
    def loop(l: MyList[A], acc: MyList[A]): MyList[A] = {
      implicit val filterPredicate: A => Filter.Filter = predicate
      l match {
        case Nil                                       => reverse(acc)
        case Cons(head, tail) if checkPredicate(head)  => loop(tail, Cons(head, acc))
        case Cons(head, Nil) if checkPredicate(head)   => loop(Nil, Cons(head, acc))
        case Cons(head, tail) if !checkPredicate(head) => loop(tail, acc)
        case Cons(head, Nil) if !checkPredicate(head)  => loop(Nil, acc)
      }
    }

    loop(myList, Nil)
  }

  object Filter {
    sealed trait Filter

    case object Skip extends Filter

    case object Preserve extends Filter
  }
}
