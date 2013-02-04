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
      val expected = "data(profile(" +"_," + FIELDS + "(firstName(\"John\"),lastName(_),description(_),emailAddress(_),country(_),region(_),city(_),postalCode(_),website(_),imageHashCode(_))))"
      val test: Profile = new Profile()
      test.firstName = "John"
      println("given:" + test.toSearchKey)
      println("expected:" + expected)

      test.toSearchKey must be_==(expected)
    }

    "set lastname" in {
      val expected = "data(profile(" +"_," + FIELDS + "(firstName(_),lastName(\"Adams\"),description(_),emailAddress(_),country(_),region(_),city(_),postalCode(_),website(_),imageHashCode(_))))"
      val test: Profile = new Profile()
      test.lastName = "Adams"
      println(test.toSearchKey)

      test.toSearchKey must be_==(expected)
    }

    "set all wildcards" in {
      //val expected = "profile(data(_," + "fields(firstName(_),lastName(_),description(_),emailAddress(_),country(_),region(_),city(_),postalCode(_),website(_),imageHashCode(_))))"
      val expected = "data(profile(_,_))"
      val test: Profile = new Profile()
      println(test.toSearchKey)
      test.toSearchKey must be_==(expected)
    }

    "set locale" in {
      //val expected = "profile(data("  + KEYS + "(id(_),localeCode(\"en\"),recVerNum(_))," + "fields(firstName(_),lastName(_),description(_),emailAddress(_),country(_),region(_),city(_),postalCode(_),website(_),imageHashCode(_))))"
      val expected = "data(profile("  + KEYS + "(id(_),localeCode(\"en\"),recVerNum(_)),_))"
      val test: Profile = new Profile()
      test.localeCode = "en"
      println(test.toSearchKey)
      test.toSearchKey must be_==(expected)
    }

    "generate search key correctly for an embedded Image" in {
      val data: Image = new Image("name", "", "content hasn't be visible in a toStoreKey", "")
      val expected = "data(profile(_," + FIELDS + "(firstName(_),lastName(\"Adams\"),description(_),emailAddress(_),country(_),region(_),city(_),postalCode(_),website(_),imageHashCode(_))))"
      val test: Profile = new Profile()
      test.imageHashCode = ""
      test.lastName = "Adams"
      //      println("given:" + test.toSearchKey)
      //      println("expected:" + expected)
      test.toSearchKey must be_==(expected)
    }

    "set all wildcards at the child level" in {
      val id = UUID.randomUUID.toString
      //val expected = "connection(data(" + KEYS + "(id(\"" + id + "\"),localeCode(_),recVerNum(_))," + FIELDS + "(category(_),connectionType(_),alias(_),readCnxn(_),writeCnxn(_),autoApprove(_),policies(_),created(_))))"
      val expected = "data(connection(" + KEYS + "(id(\"" + id + "\"),localeCode(_),recVerNum(_)),_))"
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

      val id = UUID.randomUUID.toString
      //val expected = "post(data(" + KEYS + "(id(\"" + id + "\"),localeCode(_),recVerNum(_))," + FIELDS + "(subject(_),body(_),toDetails(_),fromDetails(_),threadId(_),ignored(_),archived(_),sent(_),delivered(_),viewed(_))))"
      val expected = "data(post(" + KEYS + "(id(\"" + id + "\"),localeCode(_),recVerNum(_)),_))"
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
      val msg = new CreateInvitationRequest(null, "", "", "", "", "", "", "", postToTarget, postToBroker, false);
      val persistedMessage = new PersistedMessage[ CreateInvitationRequest ](msg)

      val searchKey = persistedMessage.toSearchKey
      println("searchKey: " + searchKey)
      //to aggressive, should be like
      //      val expected = "persistedMessage(" + FIELDS + "(id(_),localeCode(_),message(_),persisted(_)))"
      val expected = "data(persistedMessage(_,_))"
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
      val id = UUID.fromString("99595a09-8f3b-48a9-ad6d-ccd5d2782e71").toString
      //val expectedSearchKey = "connection(data(" + KEYS + "(id(\"99595a09-8f3b-48a9-ad6d-ccd5d2782e71\"),localeCode(_),recVerNum(_))," + "fields(category(\"Person\"),connectionType(\"connectionType\"),alias(\"alias\"),readCnxn(_),writeCnxn(_),autoApprove(\"autoApprove\"),policies(\"[referralsDisabled, searchDisabled]\"),created(_))))"
      //because there is an id expected result is
      val expectedSearchKey = "data(connection(" + KEYS + "(id(\"99595a09-8f3b-48a9-ad6d-ccd5d2782e71\"),localeCode(_),recVerNum(_)),_))"
      val data = new Connection(ConnectionCategory.Person.toString, "connectionType", "alias", null, null, "autoApprove", List(ConnectionPolicy.ReferralsDisabled.toString, ConnectionPolicy.SearchDisabled.toString), null)
      data.id = id
      val searchKey = data.toSearchKey
      searchKey must be_==(expectedSearchKey)
    }

    "generate search key correctly for an Image" in {
      val id = UUID.fromString("99595a09-8f3b-48a9-ad6d-ccd5d2782e71").toString
      //val expectedSearchKey = "image(data(" +  KEYS + "(id(\"99595a09-8f3b-48a9-ad6d-ccd5d2782e71\"),localeCode(_),recVerNum(_))," + "fields(name(_),contentType(_),content(_),metadata(_))))"
      val expectedSearchKey = "data(image(" +  KEYS + "(id(\"99595a09-8f3b-48a9-ad6d-ccd5d2782e71\"),localeCode(_),recVerNum(_)),_))"
      val data: Image = new Image()
      data.id = id
      val searchKey = data.toSearchKey
      searchKey must be_==(expectedSearchKey)

      val emptyData = new Image()
      val emptySearchKey = emptyData.toSearchKey
      val expectedEmptySearchKey = "data(image(_,_))"
      emptySearchKey must be_==(expectedEmptySearchKey)
    }


    "generate search key correctly for a Post" in {
      val fromDetails = new java.util.HashMap[ String, Data ]();
      val data: Post =  new Post("subject2Target", "body", fromDetails);
      data.id = id
      data.threadId = id
      val expectedSearchKey = "data(post(" +  KEYS + "(id(\"99595a09-8f3b-48a9-ad6d-ccd5d2782e71\"),localeCode(_),recVerNum(_)),_))"
      val searchKey = data.toSearchKey
      searchKey must be_==(expectedSearchKey)

      val emptyData = new Post()
      val emptySearchKey = emptyData.toSearchKey
      val expectedEmptySearchKey = "data(post(_," + FIELDS + "(subject(_),body(_),toDetails(_),fromDetails(_),threadId(_),ignored(_),archived(_),sent(_),delivered(_),viewed(_))))"
      emptySearchKey must be_==(expectedEmptySearchKey)
    }

  }

}
