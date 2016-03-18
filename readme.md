# Y-Lex Dictionary Lookup Service
Written for the ARCLITE Lab, Brigham Young University.
Contributors: Joshua Monson, Logan Kearsley, Santiago Verdu, Dagan Holbrook, David Ostler

## About
This is a web service for providing multilingual dictionary lookups.
It provides an HTTP interface for accessing a variety of dictionaries and translation services with a standardized lexicographical response format.

## Installation
This is a Play 2.4 application. See http://www.playframework.org/ for instructions on how to run and deploy a Play application.
See conf/application.conf.example for a template of the available and required configuration options.

## Adding Dictionaries
In addition to accessing external services like WordReference and Google Translate, Y-Lex makes use of its own local dictionaries to reduce xternal API usage. The dictionaries the ARCLITE lab uses are property of Brigham Young University, but you can create and add your own.

As of version 1.0, a dictionary is the following Scala type:

    TrieMap[String, ListBuffer[String]]

The following class handles serializing and deserializing dictionaries and has been provided for your convenience:

    edu.byu.arclite.dictionary.Dictionary

You can use the <code>Dictionary.saveToFile(dictionary, file)</code> method to save the dictionary to a file. When TrieMaps are serialized and saved into files, they should be saved under the <code>dictionaries</code> folder (although this default can be overridden byt setting "byu.dictpath" in the config file). The naming convention is:

    {srcLang}-{destLang}.bin

So a dictionary going from English to French would be saved here: <code>dictionaries/eng-fra.bin</code>.