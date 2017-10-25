package edu.towson.cis.cosc455.jchen21.project1

/**
  * Created by jchen21 on 10/11/2017.
  */
trait LexicalAnalyzer {
  def addChar() : Unit
  def getChar() : Char
  def getNextToken() : Unit
  def lookup() : Boolean
}
