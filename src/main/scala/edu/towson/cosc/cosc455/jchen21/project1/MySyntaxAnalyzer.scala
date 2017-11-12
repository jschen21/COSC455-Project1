package edu.towson.cosc.cosc455.jchen21.project1

import scala.collection.mutable.Stack

class MySyntaxAnalyzer extends SyntaxAnalyzer{

  var stack = Stack[String]()

  override def gittex(): Unit = {
    if (Compiler.currentToken.equalsIgnoreCase(CONSTANTS.DOCB)){
      parseTree()
      variableDefine()
      title()
      body()
      if(Compiler.currentToken.equalsIgnoreCase(CONSTANTS.DOCE)){
        parseTree()
        Compiler.Scanner.getNextToken()
      }
      else{
        error()
      }
      println(stack)
    }
    else {
      System.exit(2)
    }
  }

  override def paragraph(): Unit = {
    if(Compiler.currentToken.equalsIgnoreCase(CONSTANTS.PARAB)){
      parseTree()
      variableDefine()
      innerText()
      if(Compiler.currentToken.equalsIgnoreCase(CONSTANTS.PARAE)){
        parseTree()
      }
      else{
        error()
      }
    }
    else{
      error()
    }
  }

  override def innerItem(): Unit = {
    if(Compiler.currentToken.equalsIgnoreCase(CONSTANTS.USEB)){
      variableUse()
      innerItem()
    }
    if(Compiler.currentToken.equalsIgnoreCase(CONSTANTS.BOLD)){
      bold()
      innerItem()
    }
    if(Compiler.currentToken.equalsIgnoreCase(CONSTANTS.LINKB)){
      link()
      innerItem()
    }
    if(CONSTANTS.validText.exists(x => x.equalsIgnoreCase(Compiler.currentToken))){
      reqText()
      innerItem()
    }
  }

  override def innerText(): Unit = {
    if(Compiler.currentToken.equalsIgnoreCase(CONSTANTS.USEB)){
      variableUse()
      innerText()
    }
    if(Compiler.currentToken.equalsIgnoreCase(CONSTANTS.HEADING)){
      heading()
      innerText()
    }
    if(Compiler.currentToken.equalsIgnoreCase(CONSTANTS.BOLD)){
      bold()
      innerText()
    }
    if(Compiler.currentToken.equalsIgnoreCase(CONSTANTS.LISTITEM)){
      listItem()
      innerText()
    }
    if(Compiler.currentToken.equalsIgnoreCase(CONSTANTS.IMAGEB)){
      image()
      innerText()
    }
    if(Compiler.currentToken.equalsIgnoreCase(CONSTANTS.LINKB)){
      link()
      innerText()
    }
    if(Compiler.currentToken.equalsIgnoreCase(CONSTANTS.USEB)){
      variableUse()
      innerText()
    }
    if(isText()){
      parseTree()
      innerText()
    }
  }

  override def link(): Unit = {
    if(Compiler.currentToken.equalsIgnoreCase(CONSTANTS.LINKB)){
      parseTree()
      reqText()
      if(Compiler.currentToken.equalsIgnoreCase(CONSTANTS.BRACKETE)){
        parseTree()
        if(Compiler.currentToken.equalsIgnoreCase(CONSTANTS.ADDRESSB)){
          parseTree()
          reqText()
          if(Compiler.currentToken.equalsIgnoreCase(CONSTANTS.ADDRESSE)){
            parseTree()
          }
          else{
            error()
          }
        }
        else{
          error()
        }
      }
      else{
        error()
      }
    }
    else{
      error()
    }
  }


  override def body(): Unit = {
    if(CONSTANTS.innerText.exists(x => x.equalsIgnoreCase(Compiler.currentToken))){
      innerText()
      body()
    }
    if(Compiler.currentToken.equalsIgnoreCase(CONSTANTS.PARAB)){
      paragraph()
      body()
    }
    if(Compiler.currentToken.equalsIgnoreCase(CONSTANTS.NEWLINE)){
      newline()
      body()
    }
  }

  override def bold(): Unit = {
    if(Compiler.currentToken.equalsIgnoreCase(CONSTANTS.BOLD)){
      parseTree()
      getText()
      if(Compiler.currentToken.equalsIgnoreCase(CONSTANTS.BOLD)){
        parseTree()
      }
      else{
        error()
      }
    }
    else{
      error()
    }
  }

  override def newline(): Unit = {
    if(Compiler.currentToken.equalsIgnoreCase(CONSTANTS.NEWLINE)){
      parseTree()
    }
  }

  override def title(): Unit = {
    if(Compiler.currentToken.equalsIgnoreCase(CONSTANTS.TITLEB)){
      parseTree()
      reqText()
      if(Compiler.currentToken.equalsIgnoreCase(CONSTANTS.BRACKETE)){
        parseTree()
      }
      else{
        error()
      }
    }
    else{
      error()
    }
  }

  override def variableDefine(): Unit = {
    if(Compiler.currentToken.equalsIgnoreCase(CONSTANTS.DEFB)){
      parseTree()
      addVar()
      getText()
      if(Compiler.currentToken.equalsIgnoreCase(CONSTANTS.EQSIGN)){
        parseTree()
        addVar()
        if(Compiler.currentToken.equalsIgnoreCase(CONSTANTS.BRACKETE)){
          parseTree()
        }
        else{
          error()
        }
      }
      else{
        error()
      }
      variableDefine()
    }
  }

  override def image(): Unit = {
    if(Compiler.currentToken.equalsIgnoreCase(CONSTANTS.IMAGEB)){
      parseTree()
      reqText()
      if(Compiler.currentToken.equalsIgnoreCase(CONSTANTS.BRACKETE)){
        parseTree()
        getText()
        if(Compiler.currentToken.equalsIgnoreCase(CONSTANTS.ADDRESSB)){
          parseTree()
          reqText()
          if(Compiler.currentToken.equalsIgnoreCase(CONSTANTS.ADDRESSE)){
            parseTree()
          }
          else{
            error()
          }
        }
        else{
          error()
        }
      }
      else{
        error()
      }
    }
    else{
      error()
    }
  }

  override def variableUse(): Unit = {
    if(Compiler.currentToken.equalsIgnoreCase(CONSTANTS.USEB)){
      parseTree()
      addVar()
      if(Compiler.currentToken.equalsIgnoreCase(CONSTANTS.BRACKETE)) parseTree()
      else error()
    }
  }

  override def heading(): Unit = {
    if(Compiler.currentToken.equalsIgnoreCase(CONSTANTS.HEADING)){
      parseTree()
      reqText()
    }
  }

  override def listItem(): Unit = {
    if(Compiler.currentToken.equalsIgnoreCase(CONSTANTS.LISTITEM)){
      parseTree()
      innerItem()
      listItem()
    }
  }

  def addVar(): Unit = {
    var variable: String = ""
    while(CONSTANTS.validText.exists(x => x.equalsIgnoreCase(Compiler.currentToken))){
      if(!CONSTANTS.whiteSpace.exists(x => x.equalsIgnoreCase(Compiler.currentToken))) variable += Compiler.currentToken
      Compiler.Scanner.getNextToken()
    }
    stack.push(variable)
  }

  def reqText(): Unit = {
    if(isText()){
      getText()
    }
    else{
      error()
    }
  }

  def getText(): Unit = {
    while(isText()){
      parseTree()
    }
  }

  def isText(): Boolean = {
    if (Compiler.currentToken.contains(':') || Compiler.currentToken.contains('.') || Compiler.currentToken.contains(',')) {
      return true
    }
    if (Compiler.currentToken.contains("\n")) return Compiler.currentToken.length == Compiler.currentToken.filter(_.isLetterOrDigit).length + 1
    Compiler.currentToken.length == Compiler.currentToken.filter(_.isLetterOrDigit).length
  }


  def parseTree(): Unit = {
    stack.push(Compiler.currentToken)
    Compiler.Scanner.getNextToken()
  }

  def error(): Unit = {
    println("SYNTAX ERROR: " + "'" + Compiler.currentToken + "'" + " is not supposed to be there")
    System.exit(1)
  }
}