package controllers

import scala.concurrent.{ExecutionContext, Future, Await}
import scala.concurrent.duration._
import play.api.libs.json._
import play.api.libs.ws.WS
import play.api.Play.{current, configuration}
import ExecutionContext.Implicits.global

import xml.{Elem, XML}
import java.io.InputStream;
import java.net.URL;

object LookupMerriamWebster extends Translator {
  val name = "Merriam-Webster Inc."
  val expiration = Utils.getExpiration("merriamWebster")
  val merriamWebsterSpanishKey = configuration.getString("merriamWebster.spanishKey")
  val merriamWebsterCollegiateKey = configuration.getString("merriamWebster.collegiateKey")
    
  def parseXMLResponse(URL:String, headWordTag:String) : Seq[String] = {
    val (true, body) = try {
      val url = new URL(URL)
      val body = url.openStream
      (true, body)
    } catch { case ex:Exception => (false, null) }
	
    val XMLDoc = XML.load(body)
    val response = (XMLDoc \\ "entry_list") 
    val defList : Seq[String] = for {
      entry <- response \\ "entry"
      defins <- entry \\ "dt"
      } yield {   
        val word = (entry \\ headWordTag).text
        val defns = defins.text
        "(" + word + ") " + defns
    }
    defList
  }
  
  /**
   * Endpoint for translating via merriamWebster
   */
  def translate(src: String, dest: String, text: String) = {
    if((src == "es" && dest == "en") || (src == "en" && dest == "es")){
      merriamWebsterSpanishKey.flatMap { key => 
        val url =  "http://www.dictionaryapi.com/api/v1/references/spanish/xml/" + text +"?key=" + key;
        val result = Await.result(WS.url(url).get(), Duration.Inf)
    
          if(result.status != 200) None
          else {
            val defList : Seq[String] = parseXMLResponse(url, "hw")

            if(defList.length > 0)
              Some(defList)
            else None
          }
      }
    }
    else if(src == "en" && dest == "en"){
      merriamWebsterCollegiateKey.flatMap { key => 
        val url =  "http://www.dictionaryapi.com/api/v1/references/collegiate/xml/" + text +"?key=" + key;
        val result = Await.result(WS.url(url).get(), Duration.Inf)
    
        if(result.status != 200) None
        else {            
          val defList : Seq[String] = parseXMLResponse(url, "ew")
                
          if(defList.length > 0)
            Some(defList)
          else None
        }
      }
    }
    else None
  }
}