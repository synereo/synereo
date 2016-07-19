package com.biosimilarity.lift.test

import java.util.regex.{Pattern => RegexPtn}

import com.biosimilarity.lift.lib.{BasicLogService, ConfigurationTrampoline, WireTap}
import com.biosimilarity.lift.model.ApplicationDefaults
import com.biosimilarity.lift.model.store.MonadicTupleSpace

import scala.collection.mutable

/**
  * lifted from MonadicTSpace.scala
  */
object MonadicRegexTSpace extends MonadicTupleSpace[String, String, String] with WireTap with ConfigurationTrampoline with Serializable {

  override type Substitution = IdentitySubstitution

  override val theMeetingPlace  = new mutable.HashMap[String, String]()
  override val theChannels      = new mutable.HashMap[String, String]()
  override val theWaiters       = new mutable.HashMap[String, List[RK]]()
  override val theSubscriptions = new mutable.HashMap[String, List[RK]]()

  override def tap[A](fact: A): Unit = BasicLogService.reportage(fact)

  override def configFileName: Option[String] = None

  override def configurationDefaults: ConfigurationDefaults = ApplicationDefaults.asInstanceOf[ConfigurationDefaults]

  def representative(ptn: String): String = ptn

  def fits(ptn: String, place: String): Boolean = RegexPtn.matches(ptn, place) || RegexPtn.matches(place, ptn)

  def fitsK(ptn: String, place: String): Option[Substitution] = {
    //println( "in fitsK on " + this )
    if (fits(ptn, place))
      Some(IdentitySubstitution())
    else
      None
  }
}
