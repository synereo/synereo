package com.protegra_ati.agentservices.core.schema

import java.util.HashMap
import scala.reflect.BeanProperty
import scala.collection.JavaConversions._
import java.util.{Properties}


// mock Personal profile data object
case class CompositeMockProfile1(
  @BeanProperty var firstName: String,
  @BeanProperty var lastName: String,
  @BeanProperty var description: String,
  @BeanProperty var emailAddress: String,
  @BeanProperty var country: String,
  @BeanProperty var region: String,
  @BeanProperty var city: String,
  @BeanProperty var postalCode: String,
  @BeanProperty var image: MockImage
  )
  extends MockData
{

  def this(firstName: String
    , lastName: String
    , description: String
    , emailAddress: String
    , country: String
    , region: String
    , city: String
    , postalCode: String) = this(firstName, lastName, description, emailAddress, country, region, city, postalCode, new MockImage())

  //need a no-parameter constructor to create a version of the object with only authorized fields populated
  def this() = this("", "", "", "", "", "", "", "", new MockImage())

}

object CompositeMockProfile1
{

}
