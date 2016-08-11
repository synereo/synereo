package com.biosimilarity.evaluator.importer.utils.mailinator;

import com.biosimilarity.evaluator.importer.utils.mailinator.Email.EmailPart;
import java.io.IOException;
import java.io.InputStream;
import java.io.InputStreamReader;
import java.io.Reader;
import java.net.MalformedURLException;
import java.net.URL;
import java.net.URLConnection;
import java.util.ArrayList;
import java.util.Collections;
import java.util.HashMap;
import java.util.List;
import java.util.Map.Entry;
import java.util.Set;
import org.json.simple.JSONArray;
import org.json.simple.JSONObject;
import org.json.simple.JSONValue;

/**
 * @author Adam Boulton
 */
public class Mailinator {

  private static final String MAILINATOR_API_ENDPOINT = "https://api.mailinator.com/api";
  private static final String MAILINATOR_INBOX_TEMPLATE_URL = MAILINATOR_API_ENDPOINT + "/inbox?token=%s&to=%s&private_domain=%b";
  private static final String MAILINATOR_EMAIL_TEMPLATE_URL = MAILINATOR_API_ENDPOINT + "/email?token=%s&msgid=%s&private_domain=%b";
  private static final String MAILINATOR_DELETE_TEMPLATE_URL = MAILINATOR_API_ENDPOINT + "/delete?token=%s&msgid=%s&private_domain=%b";

  //No instantiation
  private Mailinator() {
  }

    /*
     ===== PUBLIC METHODS =====
     */
  /**
   * Retrieves all messages from an inbox for a public Mailinator domain
   *
   * @param apikey - Mailinator API key
   * @param emailAddress - Email address of the account
   * @return Array of messages from the inbox
   * @throws IOException When invalid response, for example using an invalid
   * API key
   */
  public static List<InboxMessage> getInboxMessages(String apikey, String emailAddress) throws IOException {
    return getInboxMessages(apikey, emailAddress, false);
  }

  /**
   * Retrieves all messages from an inbox for a public or private Mailinator domain
   *
   * @param apikey - Mailinator API key
   * @param emailAddress - Email address of the account
   * @param privateDomain - indicates if the request is to an account with a private domain
   * @return List of messages from the inbox
   * @throws IOException When invalid response, for example using an invalid
   * API key
   */
  public static List<InboxMessage> getInboxMessages(String apikey, String emailAddress, boolean privateDomain) throws IOException {

    ArrayList<InboxMessage> messages = new ArrayList<InboxMessage>();
    Reader reader = getInboxStream(apikey, emailAddress, privateDomain);

    JSONObject obj = (JSONObject) JSONValue.parse(reader);

    JSONArray jsonMessages = (JSONArray) obj.get("messages"); //messages array

    if(jsonMessages != null) {
      //Parse the messages, make the POJOs and add them to our custom list.
      for (Object jsonMsg : jsonMessages) {
        InboxMessage message = createInboxMessageFrom((JSONObject) jsonMsg);
        messages.add(message);
      }

      //The messages come in the response in a reversed order, so lets give
      //back an ordered list
      Collections.reverse(messages);
    }

    return messages;
  }

  /**
   * Once you have the email id's from a given inbox query, you can retrieve
   * the full email. You can use this for public Mailinator domains only.
   *
   * @param apikey - Mailinator API key
   * @param emailId- ID of email to get, can be retrieved using the
   *     {@link #getInboxMessages(String, String)} method
   * @return Email populated with data from API call
   * @throws java.io.IOException
   */
  public static Email getEmail(String apikey, String emailId) throws IOException {
    return getEmail(apikey, emailId, false);
  }

  /**
   * Once you have the email id's from a given inbox query, you can retrieve
   * the full email. You can use either a public or a private Mailinator domain.
   *
   * @param apikey - Mailinator API key
   * @param emailId - ID of email to get, can be retrieved using the
   *     {@link #getInboxMessages(String, String, boolean)} method
   * @param privateDomain - indicates if the request is to an account with a private domain
   * @return Email populated with data from API call
   * @throws java.io.IOException
   */
  public static Email getEmail(String apikey, String emailId, boolean privateDomain) throws IOException {

    String emailUrl = String.format(MAILINATOR_EMAIL_TEMPLATE_URL, apikey, emailId, privateDomain);
    Reader reader = getResponse(emailUrl);

    JSONObject obj = (JSONObject) JSONValue.parse(reader);
    return createEmailFrom(obj);
  }

  /**
   * Once you have the email id's from a given inbox query, you can delete
   * the email. You can use this for either public or private Mailinator domains.
   *
   * @param apikey - Mailinator API key
   * @param emailId - ID of email to get, can be retrieved using the
   *     {@link #getInboxMessages(String, String, boolean)} method
   * @return true if API returns OK, otherwise false
   * @throws java.io.IOException
   */
  public static boolean deleteEmail(String apikey, String emailId) throws IOException {
    return deleteEmail(apikey, emailId, false);
  }

  /**
   * Once you have the email id's from a given inbox query, you can delete
   * the email. You can use this for either public or private Mailinator domains.
   *
   * @param apikey - Mailinator API key
   * @param emailId - ID of email to get, can be retrieved using the
   *     {@link #getInboxMessages(String, String, boolean)} method
   * @param privateDomain - indicates if the request is to an account with a private domain
   * @return true if API returns OK, otherwise false
   * @throws java.io.IOException
   */
  public static boolean deleteEmail(String apikey, String emailId, boolean privateDomain) throws IOException {

    String emailUrl = String.format(MAILINATOR_DELETE_TEMPLATE_URL, apikey, emailId, privateDomain);
    Reader reader = getResponse(emailUrl);

    JSONObject obj = (JSONObject) JSONValue.parse(reader);

    return String.valueOf(obj.get("status")).equalsIgnoreCase("ok");
  }

    /*
     ===== PRIVATE METHODS =====
     */
  /**
   * Invokes the Mailnator API endpoint for the Inbox.
   *
   * @param apikey
   * @param emailAddress
   * @return The stream ready for reading the response..
   * @throws IOException
   */
  private static Reader getInboxStream(String apikey, String emailAddress, boolean privateDomain) throws IOException {
    String inboxUrl = String.format(MAILINATOR_INBOX_TEMPLATE_URL, apikey, emailAddress, privateDomain);
    Reader reader = getResponse(inboxUrl);
    return reader;
  }

  private static Reader getResponse(String url) throws MalformedURLException, IOException {
    URLConnection connection = new URL(url).openConnection();

    InputStream response = connection.getInputStream();

    return new InputStreamReader(response);
  }

  /**
   * Creates a message object based on the JSON representing a message from
   * the inbox response.
   *
   * @param jsonInboxMsg
   * @return
   */
  private static InboxMessage createInboxMessageFrom(JSONObject jsonInboxMsg) {
    //  JSONObject jsonMsg = (JSONObject) jsonMsg;

    InboxMessage message = new InboxMessage();
    message.setTo(jsonInboxMsg.get("to").toString());
    message.setId(jsonInboxMsg.get("id").toString());
    message.setSeconds_ago(Long.parseLong(jsonInboxMsg.get("seconds_ago").toString()));
    message.setTime(Long.parseLong(jsonInboxMsg.get("time").toString()));
    message.setSubject(jsonInboxMsg.get("subject").toString());
    message.setFromfull(jsonInboxMsg.get("fromfull").toString());
    message.setFrom(jsonInboxMsg.get("from").toString());
    //message.setBeen_read(Boolean.parseBoolean((String) jsonInboxMsg.get("been_read")));
    if(jsonInboxMsg.get("ip") != null) {
      message.setIp(jsonInboxMsg.get("ip").toString());
    }

    return message;
  }

  private static Email createEmailFrom(JSONObject jsonEmail) {
    Email emailMsg = new Email();

    emailMsg.setApiEmailFetchesLeft(Integer.valueOf(jsonEmail.get("apiEmailFetchesLeft").toString()));

    JSONObject jsonDataSection = (JSONObject) jsonEmail.get("data");
    emailMsg.setId(jsonDataSection.get("id").toString());
    emailMsg.setSecondsAgo(Long.valueOf(jsonDataSection.get("seconds_ago").toString()));
    emailMsg.setTo(jsonDataSection.get("to").toString());
    emailMsg.setTime(Long.valueOf(jsonDataSection.get("time").toString()));
    emailMsg.setSubject(jsonDataSection.get("subject").toString());
    emailMsg.setFromFull(jsonDataSection.get("fromfull").toString());

    //headers
    JSONObject jsonHeaders = (JSONObject) jsonDataSection.get("headers");
    emailMsg.setHeaders(builderHeaders(jsonHeaders.entrySet()));

    //Parts / content
    JSONArray jsonParts = (JSONArray) jsonDataSection.get("parts");

    for (Object jsonPart1 : jsonParts) {
      EmailPart emailPart = emailMsg.new EmailPart();

      JSONObject jsonPart = (JSONObject) jsonPart1;
      JSONObject jsonPartHeaders = (JSONObject) jsonPart.get("headers");
      emailPart.setHeaders(builderHeaders(jsonPartHeaders.entrySet()));

      emailPart.setBody(jsonPart.get("body").toString());

      emailMsg.getEmailParts().add(emailPart);
    }

    return emailMsg;
  }

  private static HashMap<String, String> builderHeaders(Set<Entry> jsonHeaderEntries) {
    HashMap<String, String> headers = new HashMap<String, String>();

    for (Entry header : jsonHeaderEntries) {
      headers.put(header.getKey().toString().trim(), header.getValue().toString().trim());
    }

    return headers;
  }

}