package pureconfig
package generic
package derivation

import scala.compiletime.{constValue, erasedValue, error, summonInline}
import scala.deriving.Mirror

import pureconfig.error.{CannotConvert, ConfigReaderFailures}
import pureconfig.generic.derivation.WidenType.widen

type EnumConfigReader[A] = EnumConfigReaderDerivation.Default.EnumConfigReader[A]

trait EnumConfigReaderDerivation(transformName: String => List[String]) {

  trait EnumConfigReader[A] extends ConfigReader[A]

  object EnumConfigReader {
    inline def derived[A](using m: Mirror.SumOf[A]): EnumConfigReader[A] = {
      val values = summonCases[m.MirroredElemTypes, A]
      new EnumConfigReader[A] {
        def from(cur: ConfigCursor): ConfigReader.Result[A] =
          for {
            value <- cur.asString
            result <-
              ordinal[A](transformName, value) match {
                case Some(ord) => Right(values(ord))
                case None =>
                  for {
                    v <- cur.asConfigValue
                    result <-
                      cur.failed(
                        CannotConvert(value, constValue[m.MirroredLabel], "The value is not a valid enum option.")
                      )
                  } yield result
              }
          } yield result
      }
    }

    inline def summonCases[T <: Tuple, A]: List[A] =
      inline erasedValue[T] match {
        case _: (h *: t) =>
          (inline summonInline[Mirror.Of[h]] match {
            case m: Mirror.Singleton =>
              widen[m.MirroredMonoType, A](m.fromProduct(EmptyTuple)) :: summonCases[t, A]
            case _ => error("Enums cannot include parameterized cases.")
          })

        case _: EmptyTuple => Nil
      }

    inline def ordinal[A](
        inline transformName: String => List[String],
        inline value: String
    )(using m: Mirror.SumOf[A]) = {
      Labels
        .transformed[m.MirroredElemLabels](transformName)
        .zipWithIndex
        .collectFirst { case (ord, i) if ord.indexOf(value) >= 0 => i }
    }
  }
}

object EnumConfigReaderDerivation {
  object Default extends EnumConfigReaderDerivation(ConfigFieldMapping(PascalCase, List(KebabCase)))
}
