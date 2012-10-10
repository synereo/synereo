/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */

package com.protegra_ati.agentservices.core.schema

import disclosure.{TrustLevel}
import org.junit._
import Assert._
import org.specs._
import org.specs.runner.JUnit4
import org.specs.runner.ConsoleRunner
import java.util.UUID
import java.util.Date
import java.util.Locale
import com.protegra_ati.agentservices.core.schema._
import com.protegra_ati.agentservices.core.schema.Constants._
import com.protegra_ati.agentservices.core.schema.validator._
import scala.collection.JavaConversions._

class DataTest
  extends JUnit4(DataTestSpecs)

object DataTestSpecsRunner
  extends ConsoleRunner(DataTestSpecs)

object DataTestSpecs extends Specification
{

  "authorizedData" should {
    "remove content" in {
      val profile = new Profile("Terry", "Bunio", "test Description", "123456789@gmail.com", "CA", "Manitoba", "Winnipeg", "postalCode", "website")
      //      val authorizedProfile = profile.authorizedData(List("Profile.firstName", "Profile.lastName")).asInstanceOf[Profile]
      val authorizedProfile = profile.authorizedData(scala.List("firstName", "lastName")).asInstanceOf[ Profile ]
      assertNotNull(authorizedProfile)
      authorizedProfile.getClass.getName must be_==(profile.getClass.getName)
      authorizedProfile.firstName must be_==(profile.firstName)
      authorizedProfile.lastName must be_==(profile.lastName)
      authorizedProfile.emailAddress must be_==("")
    }

  }

  "data" should {
    "get value" in {
      val profile = new Profile("first", "last", "test Description", "12345@someProvider.com", "US", "someUSstate", "city", "postalCode", "website")
      val fieldValue = profile.getValue("firstName");
      fieldValue must be_==("first")
    }
  }

  //  "delete key" should {
  //      val conn = new MockConnection
  //      conn.id = UUID.fromString("99595a09-8f3b-48a9-ad6d-ccd5d2782e71").toString
  //      "generate delete key correctly for Data" in {
  //        val deleteKey = conn.toStoreKey
  //        println("deleteKey: " + deleteKey)
  //        //      assertEquals("mockConnection(\"Jennifer\",\"true\",\"100\",\"01-02-1901 00:00:00\",\"1.2345\",\"\",\"0\",\"false\",\"\",\"0\",\"0\",\"0.0\",\"0.0\",\"\")", storeKey)
  //        deleteKey must be_==("mockConnection(" + FIELDS + "(id(\"99595a09-8f3b-48a9-ad6d-ccd5d2782e71\"),name(_),isBlue(_),age(_),birthDate(_),amt(_),test0(_),test1(_),test2(_),test3(_),test4(_),test5(_),test6(_),test7(_),test8(_),test9(_)))")
  //      }

  //      "generate delete key correctly for a System Data" in {
  //        val systemData = new SystemData[MockConnection](conn)
  //        val deleteKey = systemData.toStoreKey
  //        println("deleteKey: " + deleteKey)
  //        //      assertEquals("mockConnection(\"Jennifer\",\"true\",\"100\",\"01-02-1901 00:00:00\",\"1.2345\",\"\",\"0\",\"false\",\"\",\"0\",\"0\",\"0.0\",\"0.0\",\"\")", storeKey)
  //        deleteKey must be_==("systemData(mockConnection(" + FIELDS + "(id(\"99595a09-8f3b-48a9-ad6d-ccd5d2782e71\"),name(_),isBlue(_),age(_),birthDate(_),amt(_),test0(_),test1(_),test2(_),test3(_),test4(_),test5(_),test6(_),test7(_),test8(_),test9(_))))")
  //      }
  //    }

}
