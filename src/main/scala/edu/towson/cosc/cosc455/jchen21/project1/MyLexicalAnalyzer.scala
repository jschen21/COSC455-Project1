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
    if(!CONSTANTS.Keyword.exists(x => x.equalsIgnoreCase(token))) false
    else true
  }

  override def getNextToken(): Unit = {
    token = ""
    getChar()
    getNonBlank()
    if(CONSTANTS.validText.exists(x => x.equalsIgnoreCase(nextChar.toString))){
      process()
    }
    else  if(CONSTANTS.specialChar.contains(nextChar)){
      addChar()
      Compiler.currentToken = token
    }
    else  if(CONSTANTS.blank.contains(nextChar)) getChar()
    else if(nextChar.equals('\\')){
      getTag()
    }
    else if(nextChar.equals('!')){
      image()
    }
    else{
      println("LEXICAL ERROR: '" + nextChar + "' is not a valid token")
      System.exit(1)
    }
    println(token)
  }

  def fileEnd(): Boolean = {
    if(pos < Compiler.fileContents.length) false
    else true
  }


  def getTag(): Unit ={
    addChar()
    do{
      getChar()
      addChar()
    }while(!tagEnd(nextChar))
    while(CONSTANTS.whiteSpace.contains(token.substring(token.length-1))) token = token.substring(0, token.length - 1)
    if(lookUp(token)){
      Compiler.currentToken = token
    }
    else{
      error()
    }
  }

  def getNonBlank(): Unit ={
    while ((CONSTANTS.blank.contains(nextChar) && !fileEnd)) getChar()
  }

  def image(): Unit ={
    addChar()
    getChar()
    if(nextChar == '['){
      addChar()
      Compiler.currentToken = token
    }
    else{
      error()
    }
  }

  def process(): Unit = {
    addChar()
    token += getText()
    Compiler.currentToken = token
  }

  def getText() : String = {
    var text: String = ""
    getChar()

    while (!fileEnd() && !CONSTANTS.blank.contains(nextChar) && !CONSTANTS.specialChar.contains(nextChar)) {
      text += nextChar
      getChar()
    }
    pos = pos - 1
    if (nextChar == '\n') {
      getChar()
    }
    if (nextChar == '\r') {
      getChar()
      if (nextChar == '\t') {
        text += nextChar
      }
    }
    return text
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

  def error(): Unit = {
    println("LEXICAL ERROR: '" + token + "' is not a valid token")
    System.exit(1)
  }
}