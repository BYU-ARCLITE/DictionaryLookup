package edu.byu.arclite.dictionary

import play.api.cache.Cache
import play.api.Play.current
import collection.concurrent.TrieMap
import collection.mutable.ListBuffer
import play.api.{Logger, Play}
import java.io.File
import java.text.Normalizer

/**
 * Created with IntelliJ IDEA.
 * User: camman3d
 * Date: 2/5/13
 * Time: 2:39 PM
 * To change this template use File | Settings | File Templates.
 */
object DictionaryCache {

  def loadDictionary(language: String) {

    // Get the information. We'll set a expiration period on the dictionary so if it's not being used it won't take up
    // memory space
    val key = "dictionary." + language
    Logger.info("Loading dictionary " + key)
    val dictionary = Dictionary.loadFromFile(new File("./dictionaries/" + language + ".bin"))
    val expiration = Play.configuration.getInt("dictionary.expiration").get

    Cache.set(key, dictionary, expiration)
  }

  def getDictionaryEntry(language: String, key: String): List[String] = {
    val dictionaryName = "dictionary." + language
    val dictionary = Cache.getAs[TrieMap[String, ListBuffer[String]]](dictionaryName)

    // Make sure the dictionary is still there. If not, reload it
    val normalizedKey = Normalizer.normalize(key, Normalizer.Form.NFC)
    if (dictionary.isDefined) {
      val entry = dictionary.get.get(normalizedKey)
      if (entry.isDefined)
        entry.get.toList
      else
        List()
    } else {
      loadDictionary(language)
      getDictionaryEntry(language, key)
    }
  }
}
