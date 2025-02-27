package exercises03.calculator

import scala.Integral.Implicits.infixIntegralOps
import scala.annotation.tailrec
// Необходимо реализовать функцию сalculate для вычисления выражений
class Calculator[T: Integral] {
  def isZero(t: T): Boolean =
    t.toInt == 0

  def calculate(expr: Expr[T]): Result[T] = {
    sealed trait Frame
    case class Eval(e: Expr[T])                                                  extends Frame
    case object CombineMul                                                       extends Frame
    case object CombinePlus                                                      extends Frame
    case object CombineMinus                                                     extends Frame
    case object CombineDiv                                                       extends Frame
    case class IfFrame(cond: T => Boolean, thenExpr: Expr[T], elseExpr: Expr[T]) extends Frame

    @tailrec
    def loop(stack: List[Frame], results: List[Result[T]]): Result[T] = stack match {
      case Nil =>
        results.headOption.getOrElse(DivisionByZero)
      case frame :: rest =>
        frame match {
          case Eval(expr) =>
            expr match {
              case Val(v) =>
                loop(rest, Success(v) :: results) // prepend делаем
              case Mul(l, r) => // поэтому переворачиваем r и l местами
                loop(Eval(r) :: Eval(l) :: CombineMul :: rest, results)
              case Plus(l, r) =>
                loop(Eval(r) :: Eval(l) :: CombinePlus :: rest, results)
              case Minus(l, r) =>
                loop(Eval(r) :: Eval(l) :: CombineMinus :: rest, results)
              case Div(l, r) =>
                loop(Eval(r) :: Eval(l) :: CombineDiv :: rest, results)
              case If(cond, exp, l, r) =>
                loop(Eval(exp) :: IfFrame(cond, l, r) :: rest, results)
            }
          case CombineMul =>
            results match {
              case Success(lv) :: Success(rv) :: tail =>
                loop(rest, Success(lv * rv) :: tail)
              case _ =>
                DivisionByZero
            }
          case CombinePlus =>
            results match {
              case Success(lv) :: Success(rv) :: tail =>
                loop(rest, Success(lv + rv) :: tail)
              case _ =>
                DivisionByZero
            }
          case CombineMinus =>
            results match {
              case Success(lv) :: Success(rv) :: tail =>
                loop(rest, Success(lv - rv) :: tail)
              case _ =>
                DivisionByZero
            }
          case CombineDiv =>
            results match {
              case Success(lv) :: Success(rv) :: tail =>
                if (isZero(rv)) DivisionByZero
                else loop(rest, Success(lv / rv) :: tail)
              case _ =>
                DivisionByZero
            }
          case IfFrame(cond, thenExpr, elseExpr) =>
            results match {
              case Success(v) :: tail =>
                if (cond(v))
                  loop(Eval(thenExpr) :: rest, tail)
                else
                  loop(Eval(elseExpr) :: rest, tail)
              case _ =>
                DivisionByZero
            }
        }
    }

    loop(List(Eval(expr)), Nil)
  }
}
