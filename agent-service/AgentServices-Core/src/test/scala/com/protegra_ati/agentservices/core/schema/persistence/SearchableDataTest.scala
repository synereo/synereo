package com.protegra_ati.agentservices.core.schema.persistence

import org.specs2.mutable._
import org.junit.runner._
import org.specs2.runner._
import java.util.UUID
import com.protegra_ati.agentservices.core.schema._
import Constants._
import scala.collection.JavaConversions._
import com.protegra_ati.agentservices.core.messages.invitation.CreateInvitationRequest

class SearchableDataTest extends SpecificationWithJUnit
{

  "fields " should {

    "set firstname" in {
      val expected = "profile(" + FIELDS + "(id(_),localeCode(_),firstName(\"John\"),lastName(_),description(_),emailAddress(_),country(_),region(_),city(_),postalCode(_),website(_),image(_)))"
      val test: Profile = new Profile()
      test.firstName = "John"
      System.err.println("given:" + test.toSearchKey)
      System.err.println("expected:" + expected)

      test.toSearchKey must be_==(expected)
    }

    "set lastname" in {
      val expected = "profile(" + FIELDS + "(id(_),localeCode(_),firstName(_),lastName(\"Adams\"),description(_),emailAddress(_),country(_),region(_),city(_),postalCode(_),website(_),image(_)))"
      val test: Profile = new Profile()
      test.lastName = "Adams"
      println(test.toSearchKey)

      test.toSearchKey must be_==(expected)
    }

    "set all wildcards" in {
      val expected = "profile(fields(id(_),localeCode(_),firstName(_),lastName(_),description(_),emailAddress(_),country(_),region(_),city(_),postalCode(_),website(_),image(_)))"
      val test: Profile = new Profile()
      println(test.toSearchKey)
      test.toSearchKey must be_==(expected)
    }

    "set locale" in {
      val expected = "profile(fields(id(_),localeCode(\"en\"),firstName(_),lastName(_),description(_),emailAddress(_),country(_),region(_),city(_),postalCode(_),website(_),image(_)))"
      val test: Profile = new Profile()
      test.localeCode = "en"
      println(test.toSearchKey)
      test.toSearchKey must be_==(expected)
    }

    "generate search key correctly for an embedded Image" in {
      val data: Image = new Image("name", "", "content hasn't be visible in a toStoreKey", "")
      val expected = "profile(" + FIELDS + "(id(_),localeCode(_),firstName(_),lastName(\"Adams\"),description(_),emailAddress(_),country(_),region(_),city(_),postalCode(_),website(_),image(_)))"
      val test: Profile = new Profile()
      test.image = data
      test.lastName = "Adams"
      //      System.err.println("given:" + test.toSearchKey)
      //      System.err.println("expected:" + expected)
      test.toSearchKey must be_==(expected)
    }

    "set all wildcards at the child level" in {
      val id = UUID.randomUUID.toString
      val expected = "connection(" + FIELDS + "(id(\"" + id + "\"),localeCode(_),category(_),connectionType(_),alias(_),readCnxn(_),writeCnxn(_),autoApprove(_),policies(_),created(_)))"
      val test = new Connection()
      test.id = id
      println(test.toSearchKey)
      test.toSearchKey must be_==(expected)
    }

    //change this to mockConnnection class yet, also deal with the Tracking Trait $$ and not being trimmed
    //    post(fields(id("3be28d16-d048-429e-b2e2-28a27204eeb6"),localeCode(_),subject(_),body(_),toDetails(_),fromDetails(_),threadId(_),com$protegra$agentserv
    //    ices$schema$behaviors$Tracking$$sent(_),com$protegra$agentservices$schema$behaviors$Tracking$$delivered(_),com$protegra$agentservices$schema$behaviors
    //    $Tracking$$viewed(_)))

    "work with traits" in {
      skipped("TODO: need to update the test")
      val id = UUID.randomUUID.toString
      val expected = "post(" + FIELDS + "(id(\"" + id + "\"),localeCode(_),subject(_),body(_),toDetails(_),fromDetails(_),threadId(_),sent(_),delivered(_),viewed(_)))"
      val test = new Post()
      test.id = id
      println(test.toSearchKey)
      test.toSearchKey must be_==(expected)
    }
    "work with a nested Message and a Hashmap" in {
      val fromDetails = new java.util.HashMap[ String, Data ]();
      val tempProfile = new Profile();
      tempProfile.setFirstName("first");
      tempProfile.setLastName("last");
      fromDetails.put(tempProfile.formattedClassName, tempProfile);

      val postToTarget = new Post("subject: to Target ", "body", fromDetails);
      val postToBroker = new Post("subject: to Broker", "body", fromDetails);
      val msg = new CreateInvitationRequest(null, "", "", "", "", "", "", postToTarget, postToBroker);
      val persistedMessage = new PersistedMessage[ CreateInvitationRequest ](msg)

      val searchKey = persistedMessage.toSearchKey
      println("searchKey: " + searchKey)
      //to aggressive, should be like
      //      val expected = "persistedMessage(" + FIELDS + "(id(_),localeCode(_),message(_),persisted(_)))"
      val expected = "persistedMessage(_)"
      searchKey must be_==(expected)
    }

    //    "work with empty hashmap" in {
    //      val id = UUID.randomUUID.toString
    //      val expected = "post(" + FIELDS + "(id(\"" + id + "\"),localeCode(_),subject(_),body(_),toDetails(_),fromDetails(_),threadId(_)))"
    //      val test = new Post()
    //      test.id = id
    //      println(test.toSearchKey)
    //      test.toSearchKey must be_==(expected)
    //    }
    //change this to mockConnnection class yet, currently skipping hashmap contents
    //    "work with full hashmap" in {
    //      val id = UUID.randomUUID.toString
    //      val expected = "post(" + FIELDS + "(id(\"" + id + "\"),localeCode(_),subject(_),body(_),toDetails(_),fromDetails(_),threadId(_)))"
    //      val test = new Post("","",new java.util.HashMap[String, Data]())
    //      test.fromDetails.put("1", new Profile())
    //      test.id = id
    //      println(test.toSearchKey)
    //      test.toSearchKey must be_==(expected)
    //    }
  }

  "toSearchKey " should {
    val id = UUID.fromString("99595a09-8f3b-48a9-ad6d-ccd5d2782e71").toString

    //Connection needs to be updated to output the right key again
    "generate search key correctly for a Connection" in {
      skipped("TODO: need to update the test")
      val id = UUID.fromString("99595a09-8f3b-48a9-ad6d-ccd5d2782e71").toString
      val expectedSearchKey = "connection(fields(id(\"99595a09-8f3b-48a9-ad6d-ccd5d2782e71\"),localeCode(_),category(\"Person\"),connectionType(\"connectionType\"),alias(\"alias\"),readCnxn(_),writeCnxn(_),autoApprove(\"autoApprove\"),policies(\"[referralDisabled, searchDisabled]\"),created(_)))"
      val data = new Connection(ConnectionCategory.Person.toString, "connectionType", "alias", null, null, "autoApprove", List(ConnectionPolicy.ReferralsDisabled.toString, ConnectionPolicy.SearchDisabled.toString), null)
      data.id = id
      val searchKey = data.toSearchKey
      searchKey must be_==(expectedSearchKey)
    }

    "generate search key correctly for an Image" in {
      val id = UUID.fromString("99595a09-8f3b-48a9-ad6d-ccd5d2782e71").toString
      val expectedSearchKey = "image(fields(id(\"99595a09-8f3b-48a9-ad6d-ccd5d2782e71\"),localeCode(_),name(_),contentType(_),content(_),metadata(_)))"
      val data: Image = new Image()
      data.id = id
      val searchKey = data.toSearchKey
      searchKey must be_==(expectedSearchKey)

      val emptyData = new Image()
      val emptySearchKey = emptyData.toSearchKey
      val expectedEmptySearchKey = "image(_)"
      emptySearchKey must be_==(expectedEmptySearchKey)
    }

  }

}
