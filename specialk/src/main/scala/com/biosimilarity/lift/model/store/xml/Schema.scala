package com.biosimilarity.lift.model.store.xml

/* User: jklassen
*/

trait Schema
{
  def toRecord( recordType : String )( key : String, value : String ) =
  {
    ( "<%RECORDTYPE%>" + key + value + "</%RECORDTYPE%>" ).replace(
      "%RECORDTYPE%", recordType
    )
  }

  def toRecords(record: String) =
  {
    "<records>" + record + "</records>"
  }

  def emptyDocument() =
  {
    "<database><records></records></database>"
  }

  //put more in here such as /records/record for rootnode, recordname, rootpath?
}
