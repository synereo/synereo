package com.biosimilarity.lift.model.store.xml

/* User: jklassen
*/

trait Schema
{
  def toRecord(key: String, value: String) =
  {
    "<record>" + key + value + "</record>"
  }

  def toRecords(record: String) =
  {
    "<records>" + record + "</records>"
  }

  //put more in here such as /records/record for rootnode, recordname, rootpath?
}