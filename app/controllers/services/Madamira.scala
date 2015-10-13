package controllers

import scala.concurrent.{ExecutionContext, Future, Await}
import scala.concurrent.duration._
import play.api.libs.json._
import play.api.libs.ws.WS
import play.api.Play.{current, configuration}
import ExecutionContext.Implicits.global
/*
object LookupMadamira extends Translator {
  val name = "Madamira"
  val madamiraURL = configuration.getString("services.madamira")

  /**
   * Endpoint for translating via Madamira
   */
  def translate(src: String, dest: String, text: String) = {
      val inputXmlData = /* to do still */
      val query = WS.url("http://localhost:8223")
        .withHeaders("Content-Type" -> "application/xml").post(inputXmlData)
      val result = Await.result(query, Duration.Inf)
      if(result.status != 200) None
      else {
        val outputXmlData = result.xml   /* to do extract translations from returns xml and restart the query
        if(translations.length > 0) Some(translations) */
        else None
      }
    
  }
} 

*/