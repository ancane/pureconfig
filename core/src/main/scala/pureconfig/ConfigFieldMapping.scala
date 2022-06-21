package pureconfig

/** A mapping between case class fields and their respective keys in the config.
  */
trait ConfigFieldMapping extends (String => List[String]) {

  def apply(fieldName: String): List[String]

  /** Returns a `ConfigFieldMapping` that uses this mapping with some overrides.
    *
    * @param overrides
    *   the overrides for this mapping as pairs (field, configKey)
    * @return
    *   a `ConfigFieldMapping` that maps fields using `overrides` if the field is present there and otherwise uses this
    *   mapping.
    */
  def withOverrides(overrides: (String, String)*) =
    ConfigFieldMapping(overrides.map { case (k, v) => k -> List(v) }.toMap.withDefault(apply))
}

object ConfigFieldMapping {

  /** Creates a ConfigFieldMapping from the provided function, mapping names in the object that will receive config
    * values to names in the configuration file.
    *
    * @param f
    *   a function that maps names in the object that will receive config values to names in the configuration file
    * @return
    *   a ConfigFieldMapping created from the provided function.
    */
  def apply(f: String => List[String]): ConfigFieldMapping =
    new ConfigFieldMapping {
      def apply(fieldName: String): List[String] = f(fieldName)
    }

  /** Creates a ConfigFieldMapping according to the naming conventions specified both for the object that will receive
    * config values and for the configuration file.
    *
    * @param typeFieldConvention
    *   naming convention used by the fields of the object which will receive config values
    * @param configFieldConvention
    *   naming convention used in the configuration file
    * @return
    *   a ConfigFieldMapping created according to the provided naming conventions.
    */
  def apply(typeFieldConvention: NamingConvention, configFieldConvention: NamingConvention): String => String = {
    if (typeFieldConvention == configFieldConvention) {
      identity
    } else {
      typeFieldConvention.toTokens _ andThen ((xs: Seq[String]) => configFieldConvention.fromTokens(xs))
    }
  }

  def apply(
      typeFieldConvention: NamingConvention,
      configFieldConventions: List[NamingConvention]
  ): ConfigFieldMapping = {
    apply(typeFieldConvention.toTokens _ andThen ((xs: Seq[String]) => configFieldConventions.map(_.fromTokens(xs))))
  }

}
