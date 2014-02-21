package controllers

import play.api.Play.{current, configuration}

trait Translator {
  val name: String
  val expiration: Int

  /**
   * Endpoint for translating
   * @param src The source language
   * @param dest The destination language
   * @param text The text to translate
   */
  def translate(src: String, dest: String, text: String): Option[Seq[String]]
}

object Utils {
  def getExpiration(prefix: String): Int = configuration.getMilliseconds(s"$prefix.expiration")
    .orElse(configuration.getMilliseconds("services.expiration"))
	.map(_.toInt / 1000)
    .getOrElse(3600*24)
}