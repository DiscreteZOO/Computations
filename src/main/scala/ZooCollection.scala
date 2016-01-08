import io.duality.PersistableSet

/**
  * Created by katja on 02/01/16.
  */
class ZooCollection[T] {

  val persistableSet = new PersistableSet[T]

  val booleanProperties = new DynamicPropertySet(PropertyType.BooleanPropertyType)
  val doubleProperties = new DynamicPropertySet(PropertyType.DoublePropertyType)
  val integerProperties = new DynamicPropertySet(PropertyType.IntegerPropertyType)
  val rationalProperties = new DynamicPropertySet(PropertyType.RationalPropertyType)
  val stringProperties = new DynamicPropertySet(PropertyType.StringPropertyType)

}
