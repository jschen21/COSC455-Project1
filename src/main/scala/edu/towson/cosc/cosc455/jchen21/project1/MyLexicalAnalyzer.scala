package edu.towson.cosc.cosc455.jchen21.project1
import edu.towson.cosc.cosc455.jchen21.project1.CONSTANTS._

class MyLexicalAnalyzer extends LexicalAnalyzer {
  var token : String = ""
  override def addChar(): Unit = ???

  override def lookup(token : String): Boolean = {
    return KEYWORD.contains(token.toUpperCase)
  }

  override def getNextToken(): Unit = {
    val c  = getChar()
  }
  def hasNextToken(): Boolean = ???

  override def getChar(): Char = ???


}
