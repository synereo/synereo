package com.biosimilarity.evaluator.importer.utils.mailinator;

import java.util.HashMap;
import java.util.HashSet;
import java.util.Set;

/**
 *
 * @author Adam
 */
public class Email {

  //  private int apiInboxFetchesLeft;
  private int apiEmailFetchesLeft;
  //  private int forwardsLeft;

  //Represents the data section in the JSON feed
  private long secondsAgo;
  private String id;
  private String to;
  private long time;
  private String subject;
  private String fromFull;

  //Represents the headers in the data section on the root of a message JSON feed
  private HashMap<String, String> headers;

  //Represents the parts section of the email JSON feed
  private Set<EmailPart> emailParts = new HashSet<EmailPart>();

  public int getApiEmailFetchesLeft() {
    return apiEmailFetchesLeft;
  }

  protected void setApiEmailFetchesLeft(int apiEmailFetchesLeft) {
    this.apiEmailFetchesLeft = apiEmailFetchesLeft;
  }

  public long getSecondsAgo() {
    return secondsAgo;
  }

  protected void setSecondsAgo(long secondsAgo) {
    this.secondsAgo = secondsAgo;
  }

  public String getId() {
    return id;
  }

  protected void setId(String id) {
    this.id = id;
  }

  public String getTo() {
    return to;
  }

  protected void setTo(String to) {
    this.to = to;
  }

  public long getTime() {
    return time;
  }

  protected void setTime(long time) {
    this.time = time;
  }

  public String getSubject() {
    return subject;
  }

  protected void setSubject(String subject) {
    this.subject = subject;
  }

  public String getFromFull() {
    return fromFull;
  }

  protected void setFromFull(String fromFull) {
    this.fromFull = fromFull;
  }

  public HashMap<String, String> getHeaders() {
    return headers;
  }

  protected void setHeaders(HashMap<String, String> headers) {
    this.headers = headers;
  }

  public Set<EmailPart> getEmailParts() {
    return emailParts;
  }

  protected void setEmailParts(Set<EmailPart> emailParts) {
    this.emailParts = emailParts;
  }

  public class EmailPart {

    private HashMap<String, String> headers;
    private String body;

    public HashMap<String, String> getHeaders() {
      return headers;
    }

    protected void setHeaders(HashMap<String, String> headers) {
      this.headers = headers;
    }

    public String getBody() {
      return body;
    }

    protected void setBody(String body) {
      this.body = body;
    }

  }

  @Override
  public String toString() {
    return "Email{" + "apiEmailFetchesLeft=" + apiEmailFetchesLeft + ", secondsAgo=" + secondsAgo + ", id=" + id + ", to=" + to + ", time=" + time + ", subject=" + subject + ", fromFull=" + fromFull + ", headers=" + headers + ", emailParts=" + emailParts + '}';
  }

}