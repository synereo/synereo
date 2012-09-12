/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */

package com.protegra_ati.agentservices.core.messages

import com.protegra_ati.agentservices.core.messages.content._
import org.junit._
import Assert._
import com.protegra_ati.agentservices.core.schema.Data

class ChannelTest {
 
  @Before
  def setUp: Unit = {
  }

  @After
  def tearDown: Unit = {
  }

  @Test
  def content = {
    var msg = new com.protegra_ati.agentservices.core.messages.content.GetContentRequest(null, null)
    assertEquals(msg.channel,Channel.Content)
  }

//  @Test
//  def survey = {
//    val msg = new SurveyRequest
//    assertEquals(msg.channel,Channel.Survey)
//  }

  @Test
  def toStringTest = {
    var msg = new GetContentRequest(null, null)
    assertEquals(msg.channel.toString, Channel.Content.toString)
  }

}
