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
  val name = "Merriam-Webster"
  val expiration = Utils.getExpiration("merriamWebster")
  val merriamWebsterKey = configuration.getString("merriamWebster.key")

    def request(urlString:String): (Boolean, InputStream) =
    try {
        val url = new URL(urlString)
        val body = url.openStream
        (true, body)
    }
    catch {
        case ex:Exception => (false, null)
    }

    def fetchAndParseURL(URL:String) = {
        val (true, body) = request(URL)
        XML.load(body)
    }
  
  /**
   * Endpoint for translating via merriamWebster
   */
  def translate(src: String, dest: String, text: String) = {

    if((src == "es" && dest == "en") || (src == "en" && dest == "es")){
        merriamWebsterKey.flatMap { key => 
            val url =  "http://www.dictionaryapi.com/api/v1/references/spanish/xml/" + text +"?key=" + key;
            val result = Await.result(WS.url(url).get(), Duration.Inf)
    
            if(result.status != 200) None   // if there is an error, return None
            else {
                
                /**
                * Testing this code
                *
                */
        
            
                val pauDoc = fetchAndParseURL(url);
          
          
                /**
                * Tryig to parse an xml string to change to json
                *
                */
                
                //def translations = Seq[String];
                
                //def run = List{"" ; "" ; ""};
                
                val response = (pauDoc \\ "entry_list") 
                val defList : Seq[String] = for {
                  entry <- response \\ "entry"
                } yield {   
                  val word = (entry \\ "hw").text
                  val defns = (entry \\ "dt").text
                  word + ": " + defns
                }
                
                //println(response);
                    //val translations = (result.json \ "data" \ "translations" \\ "translatedText").map(_.toString)
                    if(defList.length > 0)
                        Some(defList)
                    else None
            }
        }
    } 
  else None; }
}