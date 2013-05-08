package com.protegra_ati.agentservices.core.schema

import scala.beans.BeanProperty
import scala.language.existentials

/* User: mgevantmakher
*/


case class MockWithJavaMap(@BeanProperty var name: String, @BeanProperty var contentType: String, map: java.util.Map[ String, _ <: AnyRef ], map1: java.util.Map[ String, _ <: AnyRef ]) extends MockData
{
  def this() = this(null, null, null, null)


}

case object MockWithJavaMap
{


  final def simpleMockHashMapStringString =
  {
    val map = new java.util.HashMap[ String, String ]()
    map.put("KEY", "VALUE")
    val map1 = new java.util.HashMap[ String, String ]()
    map1.put("KEY1", "VALUE1")
    new MockWithJavaMap(" MockWithCollection", "some content type", map, map1)
  }

  private val mocData = new Data("id", "ca")

  final def simpleMockHashMapStringData =
  {
    val map = new java.util.HashMap[ String, Data ]()
    map.put("KEY", mocData)
    val map1 = new java.util.HashMap[ String, Data ]()
    map1.put("KEY1", mocData)
    new MockWithJavaMap(" MockWithCollection", "some content type", map, map1)
  }


  private val mocProfile = new SimpleMockProfile1("firstName", "lastName", "description", "emailAddress", "country", "region", "city", "postalCode")

  final def simpleMockHashMapStringMockProfile =
  {
    val map = new java.util.HashMap[ String, SimpleMockProfile1 ]()
    map.put("KEY", mocProfile)
    val map1 = new java.util.HashMap[ String, SimpleMockProfile1 ]()
    map1.put("KEY1", mocProfile)
    new MockWithJavaMap(" MockWithCollection", "some content type", map, map1)
  }


  private val reallyProfile = new Profile("firstName", "lastName", "description", "emailAddress", "country", "region", "city", "postalCode", "website");

  final def simpleMockHashMapStringProfile =
  {
    val map = new java.util.HashMap[ String, Profile ]()
    map.put("KEY", reallyProfile)
    val map1 = new java.util.HashMap[ String, Profile ]()
        map1.put("KEY", reallyProfile)
    new MockWithJavaMap(" MockWithCollection", "some content type", map, map1)
  }

  final def simpleMockHashMapStringProfileDownGradedToData =
  {
    val map = new java.util.HashMap[ String, Data ]()
    map.put("KEY", reallyProfile)
    val map1 = new java.util.HashMap[ String, Data ]()
        map1.put("KEY1", reallyProfile)
    new MockWithJavaMap(" MockWithCollection", "some content type", map,map1)
  }


}







