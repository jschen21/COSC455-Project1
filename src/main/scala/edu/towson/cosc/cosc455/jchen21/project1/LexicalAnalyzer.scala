package edu.towson.cosc.cosc455.jchen21.project1

trait LexicalAnalyzer {
  def addChar() : Unit
  def getChar() : Char
  def getNextToken() : Unit
  def lookUp(token : String) : Boolean
}