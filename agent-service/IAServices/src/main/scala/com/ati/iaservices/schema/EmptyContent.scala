package com.ati.iaservices.schema

case class EmptyContent()
  extends ContentValue()
{
}

object EmptyContent
{
  final val SEARCH_ALL_KEY = new Content(new EmptyContent).toSearchKey

  final val SEARCH_ALL = new Content(new EmptyContent)
  {
    override def toSearchKey(): String = EmptyContent.SEARCH_ALL_KEY
  }
}