package com.biosimilarity.magritte.json;
import com.biosimilarity.magritte.json.Absyn.*;
/** BNFC-Generated Composition Visitor
*/

public class ComposVisitor<A> implements
  com.biosimilarity.magritte.json.Absyn.JSONObject.Visitor<com.biosimilarity.magritte.json.Absyn.JSONObject,A>,
  com.biosimilarity.magritte.json.Absyn.JSONPair.Visitor<com.biosimilarity.magritte.json.Absyn.JSONPair,A>,
  com.biosimilarity.magritte.json.Absyn.JSONArray.Visitor<com.biosimilarity.magritte.json.Absyn.JSONArray,A>,
  com.biosimilarity.magritte.json.Absyn.JSONValue.Visitor<com.biosimilarity.magritte.json.Absyn.JSONValue,A>
{
/* JSONObject */
    public JSONObject visit(com.biosimilarity.magritte.json.Absyn.JObject p, A arg)
    {
      ListJSONPair listjsonpair_ = new ListJSONPair();
      for (JSONPair x : p.listjsonpair_) {
        listjsonpair_.add(x.accept(this,arg));
      }

      return new com.biosimilarity.magritte.json.Absyn.JObject(listjsonpair_);
    }

/* JSONPair */
    public JSONPair visit(com.biosimilarity.magritte.json.Absyn.JPair p, A arg)
    {
      String string_ = p.string_;
      JSONValue jsonvalue_ = p.jsonvalue_.accept(this, arg);

      return new com.biosimilarity.magritte.json.Absyn.JPair(string_, jsonvalue_);
    }

/* JSONArray */
    public JSONArray visit(com.biosimilarity.magritte.json.Absyn.JArray p, A arg)
    {
      ListJSONValue listjsonvalue_ = new ListJSONValue();
      for (JSONValue x : p.listjsonvalue_) {
        listjsonvalue_.add(x.accept(this,arg));
      }

      return new com.biosimilarity.magritte.json.Absyn.JArray(listjsonvalue_);
    }

/* JSONValue */
    public JSONValue visit(com.biosimilarity.magritte.json.Absyn.JStr p, A arg)
    {
      String string_ = p.string_;

      return new com.biosimilarity.magritte.json.Absyn.JStr(string_);
    }
    public JSONValue visit(com.biosimilarity.magritte.json.Absyn.JNum p, A arg)
    {
      Double double_ = p.double_;

      return new com.biosimilarity.magritte.json.Absyn.JNum(double_);
    }
    public JSONValue visit(com.biosimilarity.magritte.json.Absyn.JObj p, A arg)
    {
      JSONObject jsonobject_ = p.jsonobject_.accept(this, arg);

      return new com.biosimilarity.magritte.json.Absyn.JObj(jsonobject_);
    }
    public JSONValue visit(com.biosimilarity.magritte.json.Absyn.JArr p, A arg)
    {
      JSONArray jsonarray_ = p.jsonarray_.accept(this, arg);

      return new com.biosimilarity.magritte.json.Absyn.JArr(jsonarray_);
    }
    public JSONValue visit(com.biosimilarity.magritte.json.Absyn.JTru p, A arg)
    {

      return new com.biosimilarity.magritte.json.Absyn.JTru();
    }
    public JSONValue visit(com.biosimilarity.magritte.json.Absyn.JFal p, A arg)
    {

      return new com.biosimilarity.magritte.json.Absyn.JFal();
    }
    public JSONValue visit(com.biosimilarity.magritte.json.Absyn.JNul p, A arg)
    {

      return new com.biosimilarity.magritte.json.Absyn.JNul();
    }

}