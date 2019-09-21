package uk.co.anyjunk
package commons

import io.typechecked.numerology.ternary.Gte
import io.typechecked.numerology.ternary.Sum
import io.typechecked.numerology.ternary.TNat
import io.typechecked.numerology.ternary.TNat.{t0, t1}
import io.typechecked.numerology.ternary.UnsafeToInt

/**
 * A class witnessing that its internal string is less than `N` characters long
 *
 * Can only be created by the safe constructors in the companion object
 *
 * We use `UnsafeToInt` as integer evidence of `N` because we are very unlikely to have a string
 * length exceeding `Int.MaxValue`
 */
sealed abstract case class FString[N <: TNat](value: String) {

  override def toString: String = value

  def toUpperCase: FString[N] = new FString[N](value.toUpperCase) {}
  def toLowerCase: FString[N] = new FString[N](value.toLowerCase) {}

  def trim: FString[N] = new FString[N](value.trim) {}

  def ++[U2 <: TNat](that: FString[U2])(implicit sum: Sum[U2, N]): FString[sum.Out] =
    new FString[sum.Out](value ++ that.value) {}

  def up[N2 <: TNat](implicit ev: Gte[N2, N]): FString[N2] = new FString[N2](value) {}

  def truncateTo[N2 <: TNat](implicit toInt: UnsafeToInt[N2]): Either[String, FString[N2]] =
    FString.fromString[N2](value.take(toInt.value).trim)

}

object FString {

  val Empty: FString[t0] = new FString[t0]("") {}

  val WhiteSpace: FString[t1] = new FString[t1](" ") {}

  def fromString[N <: TNat](value: String)(implicit toInt: UnsafeToInt[N]): Either[String, FString[N]] = {
    val length = value.length
    val u = toInt.value
    if (length <= u) Right(new FString[N](value) {})
    else Left(s"Expected a string with length <= $u but received '$value' (length $length")
  }

  def truncateTo[N <: TNat](value: String)(implicit toInt: UnsafeToInt[N]): Either[String, FString[N]] =
    fromString(value.take(toInt.value).trim)

  def optionally[N <: TNat](value: String)(implicit toInt: UnsafeToInt[N]): Option[FString[N]] =
    fromString[N](value).toOption

}
