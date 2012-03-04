package com.biosimilarity.lift.lib.http


object HttpPimping {

  implicit def pimpedThrowable(th: Throwable) = new {
  	def toStackTrace: String = {
      val sw = new java.io.StringWriter
      val pw = new java.io.PrintWriter(sw)
      th.printStackTrace(pw)
      pw.close
      sw.close
      sw.toString
    }
  }

  implicit def pimpedString(s: String) = new {
    /**
     * this is a quick and dirty way to string escape the json to be inserted in the message as json
     * it is definitely not the most performant way to do it
     */
  	def toEscapedJson = (
  		s.replace("\\\\","\\")
  		 .replace("\"", "\\")
  		 .replace("\n", "\n")
  		 .replace("\r", "\r")
  	)

    /**
     *  replace all ocurrences of :var: in the string by looking up with the key "var" passed in in the map
     *  
     *  so "hello :a:".replaceVars(Map("a"->"world")) == "hello world"
     *   
     */
  	def replaceVars(vars: Map[String,String]) = 
      vars
        .foldLeft(s)
          { case (acc,(k,v)) => acc.replace(":" + k + ":", v) }
  }

}