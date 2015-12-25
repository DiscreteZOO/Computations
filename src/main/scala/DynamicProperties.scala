import io.duality.PersistableSet

/**
  * Created by katja on 05/12/15.
  */
object DynamicProperties {

  private def getDynamicProperties(set: Set[String], propertyType: PropertyType): PersistableSet[DynamicProperty] = {
    val setDP = new PersistableSet[DynamicProperty]
    set.foreach(name => setDP += new DynamicProperty(name, propertyType))
    setDP
  }

  val booleanProperties = getDynamicProperties(booleans, PropertyType.BooleanPropertyType)
  val doubleProperties = getDynamicProperties(doubles, PropertyType.DoublePropertyType)
  val integerProperties = getDynamicProperties(integers, PropertyType.IntegerPropertyType)
  val rationalProperties = getDynamicProperties(rationals, PropertyType.RationalPropertyType)

}
