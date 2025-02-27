package exercises03

import scala.annotation.tailrec

sealed trait Tree[+A]
final case class Leaf[A](value: A)                        extends Tree[A]
final case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]

object Tree {
  def fold[A, B](t: Tree[A])(f: A => B)(g: (B, B) => B): B = {

    sealed trait Frame
    case class Mutate(tree: Tree[A]) extends Frame
    case object MakeBranch           extends Frame

    @tailrec
    def loop(stack: List[Frame], results: List[B]): B = stack match {
      case Nil if results.nonEmpty => results.head
      case Mutate(Leaf(a)) :: rest =>
        loop(rest, f(a) :: results)
      case Mutate(Branch(l, r)) :: rest =>
        loop(Mutate(r) :: Mutate(l) :: MakeBranch :: rest, results)
      case MakeBranch :: rest =>
        results match {
          case left :: right :: tail =>
            loop(rest, g(left, right) :: tail)
        }
    }
    loop(List(Mutate(t)), Nil)
  }

  def size[A](t: Tree[A]): Int = fold(t)(_ => 1)(_ + _ + 1) // +1 учитываем сам Branch

  def max(t: Tree[Int]): Int = fold(t)(identity)(math.max)

  // максимальная глубина левого и правого поддерева + 1
  def depth[A](t: Tree[A]): Int = fold(t)(_ => 1)((l, r) => Math.max(l, r) + 1)

  // тут может пригодиться явное указание типа
  def map[A, B](t: Tree[A])(f: A => B): Tree[B] =
    fold[A, Tree[B]](t)(x => Leaf(f(x)))((x, y) => Branch(x, y))
}
