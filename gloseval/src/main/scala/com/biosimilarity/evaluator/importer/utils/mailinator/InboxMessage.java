package com.biosimilarity.evaluator.importer.utils.mailinator;

/**
 *
 * @author Adam
 */
public class InboxMessage {
  private long secondsAgo;
  private String to;
  private String id;
  private long time;
  private String subject;
  private String fromFull;
  private boolean beenRead;
  private String from;
  private String ip;


  //Auto generated getter / setters
  public long getSeconds_ago() {
    return secondsAgo;
  }

  protected void setSeconds_ago(long seconds_ago) {
    this.secondsAgo = seconds_ago;
  }

  public String getId() {
    return id;
  }

  protected void setId(String id) {
    this.id = id;
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

  public String getFromfull() {
    return fromFull;
  }

  protected void setFromfull(String fromfull) {
    this.fromFull = fromfull;
  }

  public boolean isBeen_read() {
    return beenRead;
  }

  protected void setBeen_read(boolean been_read) {
    this.beenRead = been_read;
  }

  public String getFrom() {
    return from;
  }

  protected void setFrom(String from) {
    this.from = from;
  }

  public String getIp() {
    return ip;
  }

  protected void setIp(String ip) {
    this.ip = ip;
  }

  public String getTo() {
    return to;
  }

  protected void setTo(String to) {
    this.to = to;
  }

  @Override
  public String toString() {
    return "InboxMessage{" + "secondsAgo=" + secondsAgo + ", to=" + to + ", id=" + id + ", time=" + time + ", subject=" + subject + ", fromFull=" + fromFull + ", beenRead=" + beenRead + ", from=" + from + ", ip=" + ip + '}';
  }




}