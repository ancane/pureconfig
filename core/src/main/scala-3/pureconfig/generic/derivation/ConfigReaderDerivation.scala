package pureconfig
package generic
package derivation

import scala.deriving.Mirror

trait ConfigReaderDerivation extends CoproductConfigReaderDerivation with ProductConfigReaderDerivation {
  extension (c: ConfigReader.type) {
    inline def derived[A](using m: Mirror.Of[A]): ConfigReader[A] =
      inline m match {
        case given Mirror.ProductOf[A] => derivedProduct
        case given Mirror.SumOf[A] => derivedSum
      }
  }
}

object ConfigReaderDerivation {
  object Default
      extends ConfigReaderDerivation
      with CoproductConfigReaderDerivation(ConfigFieldMapping(PascalCase, List(KebabCase)), "type")
      with ProductConfigReaderDerivation(ConfigFieldMapping(CamelCase, List(KebabCase)))

  object CamelPascal
      extends ConfigReaderDerivation
      with CoproductConfigReaderDerivation(ConfigFieldMapping(PascalCase, List(KebabCase)), "type")
      with ProductConfigReaderDerivation(ConfigFieldMapping(CamelCase, List(KebabCase, CamelCase, PascalCase)))
}

val default = ConfigReaderDerivation.Default
val camelPascal = ConfigReaderDerivation.CamelPascal
