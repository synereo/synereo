package com.biosimilarity.lift.lib.json;

import com.biosimilarity.lift.lib.json.Absyn.*;

/** BNFC-Generated All Visitor */
public interface AllVisitor<R,A> extends
  com.biosimilarity.lift.lib.json.Absyn.JSONObject.Visitor<R,A>,
  com.biosimilarity.lift.lib.json.Absyn.JSONPair.Visitor<R,A>,
  com.biosimilarity.lift.lib.json.Absyn.JSONArray.Visitor<R,A>,
  com.biosimilarity.lift.lib.json.Absyn.JSONValue.Visitor<R,A>,
  com.biosimilarity.lift.lib.json.Absyn.JSONNum.Visitor<R,A>,
  com.biosimilarity.lift.lib.json.Absyn.JSONInt.Visitor<R,A>
{}
