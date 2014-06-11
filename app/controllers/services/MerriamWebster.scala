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
        val wav = (entry \\ "wav").text
        val defns = defins.text
        "(" + word + ")" + defns
    }

    def isNum(item:String) : Boolean = {
        val bool = if ((item == '0')||(item == '1')||(item == '2')||(item == '3')||(item == '4')||(item == '5')||(item == '6')||(item == '7')||(item == '8')||(item == '9'))
          true
        false
    }

    val sound : Seq[String] = for {
      entry <- XMLDoc \\ "wav"
      } yield {   
        val file = entry.text
        val dir = if(file.substring(0,3) == "bix" )
              "bix"
            else if (file.substring(0,2) == "gg")
              "gg"
            else if (isNum(file.substring(0,1)))
              "number"
            else
              file.substring(0,1)
        val html = "<audio controls class='merriamAudio'><source src='http://media.merriam-webster.com/soundc11/" + dir + "/" + file + "' type='audio/wav'>Audio File Not Found.</audio>"
        html
    }

    /* If you want to reduce the amount of definitions or audio files, uncomment below*/ 
    val exampleSound : Seq[String] = if(sound.length > 0)
        {val snd : Seq[String] = Seq[String](sound(0)); snd} else {sound}
    val definitions : Seq[String] = if(defList.length > 5)
        {
            val defin : Seq[String] = Seq[String](defList(0), defList(1), defList(2), defList(3), defList(4))
            defin
        } else {defList}

    definitions ++ exampleSound
    
    /*val exampleSound : Seq[String] = if(sound.length > 0)
        {"<br/>Audio Examples:" +: sound} else {sound}
    defList ++ exampleSound.distinct*/
    
  }

  /**
   * Endpoint for translating via merriamWebster
   */
  def translate(src: String, dest: String, text: String) = {
    if((src == "es" && dest == "en") || (src == "en" && dest == "es")){
      merriamWebsterSpanishKey.flatMap { key => 
        val url =  "http://www.dictionaryapi.com/api/v1/references/spanish/xml/" + text.trim.replaceAll(" ","%20") +"?key=" + key;
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
        val url =  "http://www.dictionaryapi.com/api/v1/references/collegiate/xml/" + text.trim.replaceAll(" ","%20") +"?key=" + key
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