package com.biosimilarity.lift.lib.json;
import com.biosimilarity.lift.lib.json.Absyn.*;
/** BNFC-Generated Abstract Visitor */
public class AbstractVisitor<R,A> implements AllVisitor<R,A> {
/* JSONObject */
    public R visit(com.biosimilarity.lift.lib.json.Absyn.JObject p, A arg) { return visitDefault(p, arg); }
    public R visitDefault(com.biosimilarity.lift.lib.json.Absyn.JSONObject p, A arg) {
      throw new IllegalArgumentException(this.getClass().getName() + ": " + p);
    }
/* JSONPair */
    public R visit(com.biosimilarity.lift.lib.json.Absyn.JPair p, A arg) { return visitDefault(p, arg); }
    public R visitDefault(com.biosimilarity.lift.lib.json.Absyn.JSONPair p, A arg) {
      throw new IllegalArgumentException(this.getClass().getName() + ": " + p);
    }
/* JSONArray */
    public R visit(com.biosimilarity.lift.lib.json.Absyn.JArray p, A arg) { return visitDefault(p, arg); }
    public R visitDefault(com.biosimilarity.lift.lib.json.Absyn.JSONArray p, A arg) {
      throw new IllegalArgumentException(this.getClass().getName() + ": " + p);
    }
/* JSONValue */
    public R visit(com.biosimilarity.lift.lib.json.Absyn.JStr p, A arg) { return visitDefault(p, arg); }
    public R visit(com.biosimilarity.lift.lib.json.Absyn.JNum p, A arg) { return visitDefault(p, arg); }
    public R visit(com.biosimilarity.lift.lib.json.Absyn.JObj p, A arg) { return visitDefault(p, arg); }
    public R visit(com.biosimilarity.lift.lib.json.Absyn.JArr p, A arg) { return visitDefault(p, arg); }
    public R visit(com.biosimilarity.lift.lib.json.Absyn.JTru p, A arg) { return visitDefault(p, arg); }
    public R visit(com.biosimilarity.lift.lib.json.Absyn.JFal p, A arg) { return visitDefault(p, arg); }
    public R visit(com.biosimilarity.lift.lib.json.Absyn.JNul p, A arg) { return visitDefault(p, arg); }
    public R visitDefault(com.biosimilarity.lift.lib.json.Absyn.JSONValue p, A arg) {
      throw new IllegalArgumentException(this.getClass().getName() + ": " + p);
    }
/* JSONNum */
    public R visit(com.biosimilarity.lift.lib.json.Absyn.JDbl p, A arg) { return visitDefault(p, arg); }
    public R visitDefault(com.biosimilarity.lift.lib.json.Absyn.JSONNum p, A arg) {
      throw new IllegalArgumentException(this.getClass().getName() + ": " + p);
    }
/* JSONInt */
    public R visit(com.biosimilarity.lift.lib.json.Absyn.JInt p, A arg) { return visitDefault(p, arg); }
    public R visitDefault(com.biosimilarity.lift.lib.json.Absyn.JSONInt p, A arg) {
      throw new IllegalArgumentException(this.getClass().getName() + ": " + p);
    }

}
