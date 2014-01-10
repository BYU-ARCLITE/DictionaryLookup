# Dictionary Lookup Service
Written by Joshua Monson for the ARCLITE Lab, Brigham Young University.

## About
This is a web service for providing trans-lingual dictionary lookups.

Current Version: **1.0**

### What does this version do?
This provides an HTTP interface for accessing a dictionary.

## Installation
This is a Play 2.1 application. So you need the Play Framework version 2.1. Earler version don't work because they are built with older versions of Scala. Version 2.1 uses Scala 2.10 which contains the TrieMap class--needed for the dictionary structure. See http://www.playframework.org/ for instructions on how to run and deploy a Play application.

## Adding Dictionaries
The dictionaries the ARCLITE lab uses are property of Brigham Young University, but you can create and add your own.
As of version 1.0, a dictionary is the following Scala type:

    TrieMap[String, ListBuffer[String]]

The following class handles serializing and deserializing dictionaries and has been provided for your convenience:

    edu.byu.arclite.dictionary.Dictionary

You can use the <code>Dictionary.saveToFile(dictionary, file)</code> method to save the dictionary to a file. When TrieMaps are serialized and saved into files, they should be saved under the <code>dictionaries</code> folder. The naming convention is:

    {srcLang}-{destLang}.bin

So a dictionary going from English to French would be saved here: <code>dictionaries/en-fr.bin</code>.


### Configure translation keys

DictionaryLookup supports text translations using WordReference and Google Translate. To use these services you must specify the
right authentication credentials in the configuration file.

For WordReference, enter the API key:

    wordReference.key="word reference key goes here"

For Google Translate, enter the API key:

    google.key="Google Translate key goes here"