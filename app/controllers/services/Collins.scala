/*package controllers

import java.net.URLEncoder
import scala.concurrent.{ExecutionContext, Future, Await}
import scala.concurrent.duration._
import play.api.libs.json._
import play.api.libs.ws.WS
import play.api.Play.{current, configuration}
import ExecutionContext.Implicits.global
import controllers.LangCodes._

object LookupCollins extends Translator {

val name = "Collins";
val dictionaryCode = "american-learner";
val searchWord = "";
val format = "xml";

def requestEntries(key: String, scode: String, dcode: String, text: String){
val query = WS.url("https://api.collinsdictionary.com/api/v1").withQueryString-
("dictionaryCode" -> "american-learner", "searchWord" -> text, "format"-> "xml").get()

val result = Await.result(query, Duration.Inf)
val json = result.json.as[JsObject]
if(result.status != 200) None
else{}

}

}
*/