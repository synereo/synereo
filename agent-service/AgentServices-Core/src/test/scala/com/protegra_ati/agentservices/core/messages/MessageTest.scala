/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */

package com.protegra_ati.agentservices.core.messages

import scala.collection.JavaConversions._
import com.biosimilarity.lift.lib.UUIDOps
import com.protegra_ati.agentservices.core.schema._
import com.protegra_ati.agentservices.core.messages._
import com.protegra_ati.agentservices.core.messages.content._
import com.protegra_ati.agentservices.core.schema.util._
import java.lang.reflect._
import java.util.UUID
import org.junit._
import Assert._

class MessageTest {
 
  @Before
  def setUp: Unit = {
  }

  @After
  def tearDown: Unit = {
  }

  @Test
  def initRequest = {
//    val queryObject = new Search[Profile](classOf[Profile])
//
//    queryObject.setSearchFieldValue("id", "123")
//    queryObject.setSearchFieldValue("firstName", "test")
//    queryObject.setSearchFieldValue("lastName", "last")
//    queryObject.setSearchFieldValue("blueCrossNumber", "1")
    val queryObject = new com.protegra_ati.agentservices.core.schema.Profile("test", "last","test Description", "1@test.com","CA", "someCAprovince", "city", "postalCode", "website" )

    val msg = new GetContentRequest(null, queryObject)
    //msg.init(parentId)
    assertNotNull(msg.ids.id)
    assertEquals(msg.ids.parentId, msg.ids.id)
  }

  @Test
  def initResponse = {
    val parentIds = new Identification()

    val msg = new GetContentResponse(parentIds.copyAsChild(), null, List(new Profile("test","last","test Description", "1@test.com","CA", "someCAprovince", "city", "postalCode", "website" )))
    assertNotNull(msg.ids.id)
    assertEquals(parentIds.parentId, msg.ids.parentId)
  }

}
