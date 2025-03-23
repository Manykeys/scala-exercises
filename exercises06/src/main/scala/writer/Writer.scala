package writer

import typeclasses.Monoid.syntax.MonoidOps
import typeclasses._

case class Writer[Log, A](log: Log, value: A) {
  def tell(nextLog: Log)(implicit semigroup: Semigroup[Log]): Writer[Log, A] =
    Writer(semigroup.combine(log, nextLog), value)
}

object Writer {
  implicit class WriterOps[Log, A](val w: Writer[Log, A]) extends AnyVal {
    def map[B](f: A => B)(implicit m: Monoid[Log]): Writer[Log, B] =
      Writer.monad[Log].map(w)(f)

    def flatMap[B](f: A => Writer[Log, B])(implicit m: Monoid[Log]): Writer[Log, B] =
      Writer.monad[Log].flatMap(w)(f)
  }

  implicit def monad[Log: Monoid]: Monad[Writer[Log, *]] = new Monad[Writer[Log, *]] {
    override def pure[A](a: A): Writer[Log, A] = Writer(Monoid[Log].empty, a)

    override def map[A, B](fa: Writer[Log, A])(f: A => B): Writer[Log, B] = Writer(fa.log, f(fa.value))

    override def flatMap[A, B](fa: Writer[Log, A])(f: A => Writer[Log, B]): Writer[Log, B] = {
      val x = f(fa.value)
      Writer(fa.log |+| x.log, x.value)
    }
  }

  def tell[Log](log: Log): Writer[Log, Unit] = Writer(log, ())
}
