package edu.towson.cosc.cosc455.jchen21.project1


class MyLexicalAnalyzer extends LexicalAnalyzer {
  var token : String = ""
  var nextChar: Char = 0
  var pos: Int = 0

  override def getChar(): Char = {
    if(pos < Compiler.fileContents.length){
      nextChar = Compiler.fileContents.charAt(pos)
      pos = pos + 1
    }
    return nextChar
  }

  override def addChar(): Unit = {
    token += nextChar
  }

  override def lookUp(token : String): Boolean = {
    if(!CONSTANTS.KEYWORD.contains(token)) false
    return true
  }

  override def getNextToken(): Unit = {
    token = ""
    val c  = getChar()
    if(c.equals('#') || c.equals('*') || c.equals('+') || c.equals('(') || c.equals(')') || c.equals('[') || c.equals(']') || c.equals('=') || CONSTANTS.validText.exists(x => x.equalsIgnoreCase(c.toString))){
      process()
    }
    else if(c.equals('\\')){
      getTag()
    }
    else if(c.equals('!')){
      image()
    }
    else{
      println("LEXICAL ERROR: '" + c + "' is not a valid token")
      System.exit(1)
    }
  }

  def hasNextToken(): Boolean = {
    if(pos == (Compiler.fileContents.length - 1)) return false
    return true
  }


  def getTag(): Unit ={
    addChar()
    do{
      getChar()
      addChar()
    }while(!tagEnd(nextChar))
    if(token.endsWith("\n") || token.endsWith(" ") || token.endsWith("\t")) token = token.substring(0, token.length - 1)
    if(lookUp(token)){
      Compiler.currentToken = token
    }
    else{
      println("LEXICAL ERROR: " + token + "' is not a valid token")
      System.exit(1)
    }
  }

  def getNonBlank(): Unit ={
    do{
      addChar()
      getChar()
    } while (isSpace())
  }

  def image(): Unit ={
    addChar()
    getChar()
    if(nextChar == '['){
      addChar()
      Compiler.currentToken = token
    }
    else{
      println("LEXICAL ERROR: '" + token + "' is not a valid token")
      System.exit(1)
    }
  }

  def process(): Unit = {
    addChar()
    Compiler.currentToken = token
  }

  def tagEnd(c: Char): Boolean =
    c match {
      case '\n' => true
      case ' ' => true
      case '[' => true
      case '\t' => true
      case _ => false
    }
  def isSpace(): Boolean = nextChar == ' '
}