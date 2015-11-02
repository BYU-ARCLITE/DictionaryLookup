package controllers

import models.{User, ServiceLog}
import scala.concurrent.{ExecutionContext, Future, Await}
import scala.concurrent.duration._
import play.api.libs.json._
import play.api.libs.ws.WS
import play.api.Play.{current, configuration}
import ExecutionContext.Implicits.global

object LookupMadamira extends Translator {
  val name = "Madamira"
  val expiration = Utils.getExpiration("Madamira")
  val madamiraURL = configuration.getString("services.madamira")

  /**
   * Endpoint for translating via Madamira
   */

  def translate(user: User, src: String, dest: String, text: String) = {
      val inputXmlData = 
       "<?xml version=\"1.0\" encoding=\"UTF-8\">"  + 
(<madamira_input xmlns="urn:edu.columbia.ccls.madamira.configuration:0.1">
    <madamira_configuration>
        <preprocessing sentence_ids="false" separate_punct="true" input_encoding="UTF8"/>
        <overall_vars output_encoding="UTF8" dialect="MSA" output_analyses="TOP" morph_backoff="ADD_ALL"/>
        <requested_output>
            <req_variable name="PREPROCESSED" value="true" />

            <req_variable name="STEM" value="true" />
            <req_variable name="GLOSS" value="true" />
            <req_variable name="LEMMA" value="true" />
            <req_variable name="DIAC" value="true" />

            <req_variable name="ASP" value="true" />
            <req_variable name="CAS" value="true" />
            <req_variable name="ENC0" value="true" />
            <req_variable name="ENC1" value="false" />
            <req_variable name="ENC2" value="false" />
            <req_variable name="GEN" value="true" />
            <req_variable name="MOD" value="true" />
            <req_variable name="NUM" value="true" />
            <req_variable name="PER" value="true" />
            <req_variable name="POS" value="true" />
            <req_variable name="PRC0" value="true" />
            <req_variable name="PRC1" value="true" />
            <req_variable name="PRC2" value="true" />
            <req_variable name="PRC3" value="true" />
            <req_variable name="STT" value="true" />
            <req_variable name="VOX" value="true" />
            <req_variable name="BW" value="false" />
            <req_variable name="SOURCE" value="false" />
        </requested_output>
        <tokenization>
            <scheme alias="ATB" />
            <scheme alias="ATB4MT" />
            <scheme alias="MyD3">
        <!-- Same as D3 -->
        <scheme_override alias="MyD3"
                 form_delimiter="\u00B7"
                 include_non_arabic="true"
                 mark_no_analysis="false"
                 token_delimiter=" "
                 tokenize_from_BW="false">
          <split_term_spec term="PRC3"/>
          <split_term_spec term="PRC2"/>
          <split_term_spec term="PART"/>
          <split_term_spec term="PRC0"/>
          <split_term_spec term="REST"/>
          <split_term_spec term="ENC0"/>
          <token_form_spec enclitic_mark="+"
                   proclitic_mark="+"
                   token_form_base="WORD"
                   transliteration="UTF8">
            <normalization type="ALEF"/>
            <normalization type="YAA"/>
            <normalization type="DIAC"/>
            <normalization type="LEFTPAREN"/>
            <normalization type="RIGHTPAREN"/>
          </token_form_spec>
        </scheme_override>
      </scheme>
        </tokenization>
    </madamira_configuration>
    <in_doc id="ExampleDocument">
        <in_seg id="SENT1">

           {text} 

        </in_seg>
    </in_doc>
</madamira_input> ).toString;


      val query = WS.url("http://localhost:8223")
        .withHeaders("Content-Type" -> "application/xml").post(inputXmlData)
      val result = Await.result(query, Duration.Inf)
      if(result.status != 200) None
      else {
        val outputXmlData = result.xml   /* to do extract translations from returns xml and restart the query*/

        //if(translations.length > 0) Some(translations) 
        //else None
        None 
      }
    
  }
} 

