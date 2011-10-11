package com.biosimilarity.magritte.json;
import com.biosimilarity.magritte.json.Absyn.*;
/** BNFC-Generated Abstract Visitor */
public class AbstractVisitor<R,A> implements AllVisitor<R,A> {
/* JSONObject */
    public R visit(com.biosimilarity.magritte.json.Absyn.JObject p, A arg) { return visitDefault(p, arg); }
    public R visitDefault(com.biosimilarity.magritte.json.Absyn.JSONObject p, A arg) {
      throw new IllegalArgumentException(this.getClass().getName() + ": " + p);
    }
/* JSONPair */
    public R visit(com.biosimilarity.magritte.json.Absyn.JPair p, A arg) { return visitDefault(p, arg); }
    public R visitDefault(com.biosimilarity.magritte.json.Absyn.JSONPair p, A arg) {
      throw new IllegalArgumentException(this.getClass().getName() + ": " + p);
    }
/* JSONArray */
    public R visit(com.biosimilarity.magritte.json.Absyn.JArray p, A arg) { return visitDefault(p, arg); }
    public R visitDefault(com.biosimilarity.magritte.json.Absyn.JSONArray p, A arg) {
      throw new IllegalArgumentException(this.getClass().getName() + ": " + p);
    }
/* JSONValue */
    public R visit(com.biosimilarity.magritte.json.Absyn.JStr p, A arg) { return visitDefault(p, arg); }
    public R visit(com.biosimilarity.magritte.json.Absyn.JNum p, A arg) { return visitDefault(p, arg); }
    public R visit(com.biosimilarity.magritte.json.Absyn.JObj p, A arg) { return visitDefault(p, arg); }
    public R visit(com.biosimilarity.magritte.json.Absyn.JArr p, A arg) { return visitDefault(p, arg); }
    public R visit(com.biosimilarity.magritte.json.Absyn.JTru p, A arg) { return visitDefault(p, arg); }
    public R visit(com.biosimilarity.magritte.json.Absyn.JFal p, A arg) { return visitDefault(p, arg); }
    public R visit(com.biosimilarity.magritte.json.Absyn.JNul p, A arg) { return visitDefault(p, arg); }
    public R visitDefault(com.biosimilarity.magritte.json.Absyn.JSONValue p, A arg) {
      throw new IllegalArgumentException(this.getClass().getName() + ": " + p);
    }

}
