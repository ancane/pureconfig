package pureconfig.generic
package derivation

import scala.compiletime.{constValue, erasedValue, summonFrom, summonInline}
import scala.deriving.Mirror

object Labels {
  inline def transformed[T <: Tuple](
      inline transform: String => List[String]
  ): List[List[String]] =
    inline erasedValue[T] match {
      case _: (h *: t) =>
        transform(constValue[h & String]) :: transformed[t](transform)

      case _: EmptyTuple => Nil
    }
}
