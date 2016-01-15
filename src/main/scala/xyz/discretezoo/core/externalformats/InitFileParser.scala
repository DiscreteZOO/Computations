package xyz.discretezoo.core.externalformats

import io.duality.PersistableSet
import org.json.JSONObject
import xyz.discretezoo.core.{PropertyType, Property}

/**
  * Created by katja on 02/01/16.
  */

object InitFileParser {

  private val json = new JSONObject(scala.io.Source.fromFile("init.json").mkString)

  def getProperties(name: String): PersistableSet[Property[_]] = {

    val set = new PersistableSet[Property[_]]
    val propertiesJSON = InitFileParser.json.getJSONObject(name).getJSONArray("properties")

    Range(0, propertiesJSON.length).foreach(i => {
      val propertyTypeJSON = propertiesJSON.getJSONObject(i)
      val propertyType = PropertyType.getByName(propertyTypeJSON.getString("type"))
      val jsonArray = propertyTypeJSON.getJSONArray("list")
      Range(0, jsonArray.length).foreach(i => set += new Property(jsonArray.getString(i), propertyType))
    })

    set
  }

}
