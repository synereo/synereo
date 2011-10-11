package com.biosimilarity.magritte.json;

import com.biosimilarity.magritte.json.Absyn.*;

/** BNFC-Generated All Visitor */
public interface AllVisitor<R,A> extends
  com.biosimilarity.magritte.json.Absyn.JSONObject.Visitor<R,A>,
  com.biosimilarity.magritte.json.Absyn.JSONPair.Visitor<R,A>,
  com.biosimilarity.magritte.json.Absyn.JSONArray.Visitor<R,A>,
  com.biosimilarity.magritte.json.Absyn.JSONValue.Visitor<R,A>
{}
