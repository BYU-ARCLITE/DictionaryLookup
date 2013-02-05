package controllers

import play.api.mvc.{Action, Controller}
import play.api.libs.json.Json
import edu.byu.arclite.dictionary.DictionaryCache

/**
 * Created with IntelliJ IDEA.
 * User: camman3d
 * Date: 2/5/13
 * Time: 3:09 PM
 * To change this template use File | Settings | File Templates.
 */
object LookupV1 extends Controller {

  def lookup = Action {
    implicit request =>

      try {
        // Get the languages and key word
        val sourceLanguage = request.queryString("srcLang")(0)
        val destinationLanguage = request.queryString("destLang")(0)
        val word = request.queryString("word")(0)

        // Figure out which dictionary to use
        val dictionary = sourceLanguage + "-" + destinationLanguage

        // Get the entry
        val entries = DictionaryCache.getDictionaryEntry(dictionary, word)

        // Return the results
        if (!entries.isEmpty)
          Ok(Json.obj("success" -> true, "entries" -> entries)).withHeaders("Access-Control-Allow-Origin" -> "*")
        else
          NotFound(Json.obj("success" -> false, "message" -> "No dictionary entries."))
            .withHeaders("Access-Control-Allow-Origin" -> "*")
      } catch {
        case _: Throwable => BadRequest(Json.obj("success" -> false, "message" -> "Bad request."))
          .withHeaders("Access-Control-Allow-Origin" -> "*")
      }
  }

}
