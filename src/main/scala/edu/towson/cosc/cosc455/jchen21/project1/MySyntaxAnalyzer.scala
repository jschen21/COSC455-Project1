package edu.towson.cosc.cosc455.jchen21.project1
import scala.collection.mutable.Stack

class MySyntaxAnalyzer extends SyntaxAnalyzer{

  var stack = Stack[String]()

  override def gittex(): Unit = {
    if (Compiler.currentToken.equalsIgnoreCase(CONSTANTS.DOCB)){
      parseTree()
      Compiler.Scanner.getNextToken()
      variableDefine()
      title()
      body()
      if(Compiler.currentToken.equalsIgnoreCase(CONSTANTS.DOCE)){
        parseTree()
      }
      else{
        println("SYNTAX ERROR: End tag was expected when '" + Compiler.currentToken + "' was found.")
        System.exit(1)
      }
      if(Compiler.Scanner.hasNextToken()) println("SYNTAX ERROR: Tokens found after End tag.")
      println(stack)

    }
    else {
      println("SYNTAX ERROR: Begin tag was expected when '" + Compiler.currentToken + "' was found.")
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
        println("SYNTAX ERROR: Paragraph End tag was expected when '" + Compiler.currentToken + "' was found.")
        System.exit(1)
      }
    }
    else{
      println("SYNTAX ERROR: Paragraph Begin tag was expected when '" + Compiler.currentToken + "' was found.")
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
    if(CONSTANTS.validText.exists(x => x.equalsIgnoreCase(Compiler.currentToken))){
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
            println("SYNTAX ERROR: Address Close tag was expected when '" + Compiler.currentToken + "' was found.")
            System.exit(1)
          }
        }
        else{
          println("SYNTAX ERROR: Address Begin tag was expected when '" + Compiler.currentToken + "' was found.")
          System.exit(1)
        }
      }
      else{
        println("SYNTAX ERROR: Close Bracket tag was expected when '" + Compiler.currentToken + "' was found.")
        System.exit(1)
      }
    }
    else{
      println("SYNTAX ERROR: Link Begin tag was expected when when '" + Compiler.currentToken + "' was found.")
      System.exit(1)
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
        println("SYNTAX ERROR: Closing Bold tag was expected")
      }
    }
    else{
      println("SYNTAX ERROR: Opening Bold tag was expected")
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
        println("SYNTAX ERROR: Closing Title tag was expected")
        System.exit(1)
      }
    }
    else{
      println("SYNTAX ERROR: Title tag was expected " + Compiler.currentToken + " was found")
      System.exit(1)
    }
  }

  override def variableDefine(): Unit = {
    if(Compiler.currentToken.equalsIgnoreCase(CONSTANTS.DEFB)){
      parseTree()
      //variable name
      if(Compiler.currentToken.equalsIgnoreCase(CONSTANTS.EQSIGN)){
        parseTree()
        //set variable
        if(Compiler.currentToken.equalsIgnoreCase(CONSTANTS.BRACKETE)){
          parseTree()
        }
        else{
          println("brackete")
          System.exit(1)
        }
      }
      else{
        println("eqsign")
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
            println("addresse")
          }
        }
        else{
          println("addressb")
          println(stack)
          System.exit(1)
        }
      }
      else{
        println("brackete")
      }
    }
    else{
      println("imageb")
    }
  }

  override def variableUse(): Unit = ???

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
    if(CONSTANTS.validText.exists(x => x.equalsIgnoreCase(Compiler.currentToken))){
      getText()
    }
      else{
      println("reqtext")
    }
  }

  def getText(): Unit = {
    while(CONSTANTS.validText.exists(x => x.equalsIgnoreCase(Compiler.currentToken))){
      parseTree()
    }
  }

  def parseTree(): Unit = {
    stack.push(Compiler.currentToken)
    Compiler.Scanner.getNextToken()
  }
}