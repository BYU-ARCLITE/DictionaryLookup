package controllers

import edu.byu.arclite.dictionary.DictionaryCache

object LookupBYU extends Translator {

  val name = "BYU Dictionaries"

  /**
   * Endpoint for translating via BYU Dictionaries
   */
  def translate(src: String, dest: String, text: String) =
    DictionaryCache.getDictionaryEntry(src + "-" + dest, text)
}