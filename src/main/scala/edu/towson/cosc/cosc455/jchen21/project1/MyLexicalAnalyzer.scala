package edu.towson.cosc.cosc455.jchen21.project1

class MyLexicalAnalyzer extends LexicalAnalyzer {
  var token : String = ""
  var nextChar: Char = 0
  var pos: Int = 0

  //method to get each individual character from the file
  override def getChar(): Char = {
    if(pos < Compiler.fileContents.length){
      nextChar = Compiler.fileContents.charAt(pos) //assigns the character at index pos to next char
      pos = pos + 1 //increments pos
    }
    return nextChar
  }

  //adds characters to String variable token
  override def addChar(): Unit = {
    token += nextChar
  }

  //checks if the variable token is a valid keyword
  override def lookUp(token : String): Boolean = {
    if(!CONSTANTS.Keyword.exists(x => x.equalsIgnoreCase(token))) false
    else true
  }

  //primary method that is called by the Syntax Analyzer to get the next token
  override def getNextToken(): Unit = {
    token = ""
    getChar() //primes with the initial character of the token
    getNonBlank() //iterates until a nonblank is found
    if(CONSTANTS.validText.exists(x => x.equalsIgnoreCase(nextChar.toString))){ //Checks if the current character is normal text
      process()
    }
    else  if(CONSTANTS.specialChar.contains(nextChar)){ //Checks if the current character is a special character that is not part of a tag
      addChar()
      Compiler.currentToken = token
    }
    else  if(CONSTANTS.blank.contains(nextChar)) getChar() //Checks if the current character is a white space and iterates over it
    else if(nextChar.equals('\\')){ //checks if the current character is the beginning of a tag
      getTag()
    }
    else if(nextChar.equals('!')){ //Checks if the current character is the beginning of an image tag
      image()
    }
    else{
      println("LEXICAL ERROR: '" + nextChar + "' is not a valid token")
      System.exit(1)
    }
  }

  //checks if the analyzer has reached the end of the file
  def fileEnd(): Boolean = {
    if(pos < Compiler.fileContents.length) false
    else true
  }

  //adds characters until an open bracket and checks if that token is a valid tag
  def getTag(): Unit ={
    addChar()
    do{
      getChar()
      addChar()
    }while(!tagEnd(nextChar))
    while(CONSTANTS.whiteSpace.contains(token.substring(token.length-1))) token = token.substring(0, token.length - 1) //removes the whitespace if a whitespace is found after a tag
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

  //checks if the exclamation mark is followed by an open bracket
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

  //processes normal text
  def process(): Unit = {
    addChar()
    token += getText()
    Compiler.currentToken = token
  }

  //add characters until a word is formed
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

  //returns true if it is the end of a tag
  def tagEnd(c: Char): Boolean =
    c match {
      case '\n' => true
      case ' ' => true
      case '[' => true
      case '\t' => true
      case _ => false
    }


  def error(): Unit = {
    println("LEXICAL ERROR: '" + token + "' is not a valid token")
    System.exit(1)
  }
}