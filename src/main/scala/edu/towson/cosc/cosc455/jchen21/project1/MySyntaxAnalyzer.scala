package edu.towson.cosc.cosc455.jchen21.project1

import scala.collection.mutable.Stack

class MySyntaxAnalyzer extends SyntaxAnalyzer{

  var stack = Stack[String]()

  //start state of the grammar
  override def gittex(): Unit = {
    if (Compiler.currentToken.equalsIgnoreCase(CONSTANTS.DOCB)){
      parseTree()
      variableDefine()
      title()
      body()
      if(Compiler.currentToken.equalsIgnoreCase(CONSTANTS.DOCE)){
        parseTree()
        Compiler.Scanner.getNextToken()
        if(!Compiler.Scanner.fileEnd()){ //checks if there is anything after the End tag
          println("SYNTAX ERROR: Token found after End Tag")
          System.exit(1)
        }
      }
      else{
        println("SYNTAX ERROR: " + "'" + Compiler.currentToken + "'" + " was found when the End tag was expected")
        System.exit(1)
      }
    }
    else {
      println("SYNTAX ERROR: " + "'" + Compiler.currentToken + "'" + " was found when the Begin tag was expected")
      System.exit(1)
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
        println("SYNTAX ERROR: " + "'" + Compiler.currentToken + "'" + " was found when the Paragraph end tag was expected")
        System.exit(1)
      }
    }
    else{
      println("SYNTAX ERROR: " + "'" + Compiler.currentToken + "'" + " was found when the Paragraph begin tag was expected")
      System.exit(1)
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
            println("SYNTAX ERROR: " + "'" + Compiler.currentToken + "'" + " was found when the closing parenthesis was expected")
            System.exit(1)
          }
        }
        else{
          println("SYNTAX ERROR: " + "'" + Compiler.currentToken + "'" + " was found when the Opening parenthesis was expected")
          System.exit(1)
        }
      }
      else{
        println("SYNTAX ERROR: " + "'" + Compiler.currentToken + "'" + " was found when the Closing bracket was expected")
        System.exit(1)
      }
    }
    else{
      println("SYNTAX ERROR: " + "'" + Compiler.currentToken + "'" + " was found when the Link begin tag was expected")
      System.exit(1)
    }
  }


  override def body(): Unit = {
    if(CONSTANTS.innerText.exists(x => x.equalsIgnoreCase(Compiler.currentToken)) || CONSTANTS.validText.exists(x => x.equalsIgnoreCase(Compiler.currentToken.substring(0, 1)))){
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
        println("SYNTAX ERROR: " + "'" + Compiler.currentToken + "'" + " was found when the Bold tag was expected")
        System.exit(1)
      }
    }
    else{
      println("SYNTAX ERROR: " + "'" + Compiler.currentToken + "'" + " was found when the Bold tag was expected")
      System.exit(1)
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
        println("SYNTAX ERROR: " + "'" + Compiler.currentToken + "'" + " was found when the Closing bracket was expected")
        System.exit(1)
      }
    }
    else{
      println("SYNTAX ERROR: " + "'" + Compiler.currentToken + "'" + " was found when the Title tag was expected")
      System.exit(1)
    }
  }

  override def variableDefine(): Unit = {
    if(Compiler.currentToken.equalsIgnoreCase(CONSTANTS.DEFB)){
      parseTree()
      reqText()
      if(Compiler.currentToken.equalsIgnoreCase(CONSTANTS.EQSIGN)){
        parseTree()
        reqText()
        if(Compiler.currentToken.equalsIgnoreCase(CONSTANTS.BRACKETE)){
          parseTree()
        }
        else{
          println("SYNTAX ERROR: " + "'" + Compiler.currentToken + "'" + " was found when the Closing bracket tag was expected")
          System.exit(1)
        }
      }
      else{
        println("SYNTAX ERROR: " + "'" + Compiler.currentToken + "'" + " was found when the Equal sign was expected")
        System.exit(1)
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
            println("SYNTAX ERROR: " + "'" + Compiler.currentToken + "'" + " was found when the Closing parenthesis was expected")
            System.exit(1)
          }
        }
        else{
          println("SYNTAX ERROR: " + "'" + Compiler.currentToken + "'" + " was found when the Opening parenthesis was expected")
          System.exit(1)
        }
      }
      else{
        println("SYNTAX ERROR: " + "'" + Compiler.currentToken + "'" + " was found when the Closing bracket was expected")
        System.exit(1)
      }
    }
    else{
      println("SYNTAX ERROR: " + "'" + Compiler.currentToken + "'" + " was found when the Image tag was expected")
      System.exit(1)
    }
  }

  override def variableUse(): Unit = {
    if(Compiler.currentToken.equalsIgnoreCase(CONSTANTS.USEB)){
      parseTree()
      reqText()
      if(Compiler.currentToken.equalsIgnoreCase(CONSTANTS.BRACKETE)) parseTree()
      else {
        println("SYNTAX ERROR: " + "'" + Compiler.currentToken + "'" + " was found when the Closing bracket was expected")
        System.exit(1)}
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

  def reqText(): Unit = {
    if(isText()){
      getText()
    }
    else{
      println("SYNTAX ERROR: Text is required")
      System.exit(1)
    }
  }

  def getText(): Unit = {
    while(isText()){
      parseTree()
    }
  }

  def isText(): Boolean = {
    if (Compiler.currentToken.contains(":") || Compiler.currentToken.contains(".") || Compiler.currentToken.contains(",")) {
      return true
    }
    if (Compiler.currentToken.contains("\n")) return Compiler.currentToken.length == Compiler.currentToken.filter(_.isLetterOrDigit).length + 1
    Compiler.currentToken.length == Compiler.currentToken.filter(_.isLetterOrDigit).length
  }


  def parseTree(): Unit = { //pushes the current token into a stack and gets the next token
    stack.push(Compiler.currentToken)
    Compiler.Scanner.getNextToken()
  }
}