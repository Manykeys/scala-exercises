package registry.domain.model

import cats.data.Validated._
import cats.data.ValidatedNec
import cats.syntax.all._
import registry.domain.model.ValidationError.PhoneIsInvalid

case class PhoneNumber private (country: String, code: String, number: String)

object PhoneNumber {
  private val phoneRegex = raw"""(\+7|8)\s*(\d{3})\s*(\d{7})""".r
  //TODO: сделать парсинг номера телефона
  def parse(raw: String): ValidatedNec[PhoneIsInvalid, PhoneNumber] = {
    raw match {
      case phoneRegex(c, co, n) => PhoneNumber(c, co, n).validNec
      case _                    => PhoneIsInvalid.invalidNec
    }
  }

  @SuppressWarnings(Array("scalafix:DisableSyntax.throw"))
  def parseUnsafe(str: String): PhoneNumber = parse(str) match {
    case Valid(p)      => p
    case Invalid(errs) => throw errs.head
  }
}
