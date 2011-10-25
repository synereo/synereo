package com.biosimilarity.lift.lib.json;
import com.biosimilarity.lift.lib.json.Absyn.*;
/** BNFC-Generated Composition Visitor
*/

public class ComposVisitor<A> implements
  com.biosimilarity.lift.lib.json.Absyn.JSONObject.Visitor<com.biosimilarity.lift.lib.json.Absyn.JSONObject,A>,
  com.biosimilarity.lift.lib.json.Absyn.JSONPair.Visitor<com.biosimilarity.lift.lib.json.Absyn.JSONPair,A>,
  com.biosimilarity.lift.lib.json.Absyn.JSONArray.Visitor<com.biosimilarity.lift.lib.json.Absyn.JSONArray,A>,
  com.biosimilarity.lift.lib.json.Absyn.JSONValue.Visitor<com.biosimilarity.lift.lib.json.Absyn.JSONValue,A>,
  com.biosimilarity.lift.lib.json.Absyn.JSONNum.Visitor<com.biosimilarity.lift.lib.json.Absyn.JSONNum,A>,
  com.biosimilarity.lift.lib.json.Absyn.JSONInt.Visitor<com.biosimilarity.lift.lib.json.Absyn.JSONInt,A>
{
/* JSONObject */
    public JSONObject visit(com.biosimilarity.lift.lib.json.Absyn.JObject p, A arg)
    {
      ListJSONPair listjsonpair_ = new ListJSONPair();
      for (JSONPair x : p.listjsonpair_) {
        listjsonpair_.add(x.accept(this,arg));
      }

      return new com.biosimilarity.lift.lib.json.Absyn.JObject(listjsonpair_);
    }

/* JSONPair */
    public JSONPair visit(com.biosimilarity.lift.lib.json.Absyn.JPair p, A arg)
    {
      String string_ = p.string_;
      JSONValue jsonvalue_ = p.jsonvalue_.accept(this, arg);

      return new com.biosimilarity.lift.lib.json.Absyn.JPair(string_, jsonvalue_);
    }

/* JSONArray */
    public JSONArray visit(com.biosimilarity.lift.lib.json.Absyn.JArray p, A arg)
    {
      ListJSONValue listjsonvalue_ = new ListJSONValue();
      for (JSONValue x : p.listjsonvalue_) {
        listjsonvalue_.add(x.accept(this,arg));
      }

      return new com.biosimilarity.lift.lib.json.Absyn.JArray(listjsonvalue_);
    }

/* JSONValue */
    public JSONValue visit(com.biosimilarity.lift.lib.json.Absyn.JStr p, A arg)
    {
      String string_ = p.string_;

      return new com.biosimilarity.lift.lib.json.Absyn.JStr(string_);
    }
    public JSONValue visit(com.biosimilarity.lift.lib.json.Absyn.JNum p, A arg)
    {
      JSONNum jsonnum_ = p.jsonnum_.accept(this, arg);

      return new com.biosimilarity.lift.lib.json.Absyn.JNum(jsonnum_);
    }
    public JSONValue visit(com.biosimilarity.lift.lib.json.Absyn.JObj p, A arg)
    {
      JSONObject jsonobject_ = p.jsonobject_.accept(this, arg);

      return new com.biosimilarity.lift.lib.json.Absyn.JObj(jsonobject_);
    }
    public JSONValue visit(com.biosimilarity.lift.lib.json.Absyn.JArr p, A arg)
    {
      JSONArray jsonarray_ = p.jsonarray_.accept(this, arg);

      return new com.biosimilarity.lift.lib.json.Absyn.JArr(jsonarray_);
    }
    public JSONValue visit(com.biosimilarity.lift.lib.json.Absyn.JTru p, A arg)
    {

      return new com.biosimilarity.lift.lib.json.Absyn.JTru();
    }
    public JSONValue visit(com.biosimilarity.lift.lib.json.Absyn.JFal p, A arg)
    {

      return new com.biosimilarity.lift.lib.json.Absyn.JFal();
    }
    public JSONValue visit(com.biosimilarity.lift.lib.json.Absyn.JNul p, A arg)
    {

      return new com.biosimilarity.lift.lib.json.Absyn.JNul();
    }

/* JSONNum */
    public JSONNum visit(com.biosimilarity.lift.lib.json.Absyn.JDbl p, A arg)
    {
      Double double_ = p.double_;

      return new com.biosimilarity.lift.lib.json.Absyn.JDbl(double_);
    }

/* JSONInt */
    public JSONInt visit(com.biosimilarity.lift.lib.json.Absyn.JInt p, A arg)
    {
      Integer integer_ = p.integer_;

      return new com.biosimilarity.lift.lib.json.Absyn.JInt(integer_);
    }

}