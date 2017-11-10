package edu.towson.cosc.cosc455.jchen21.project1
import scala.collection.mutable.Stack

class MySyntaxAnalyzer extends SyntaxAnalyzer{

  var stack = Stack[String]()

  override def gittex(): Unit = {
    if (Compiler.currentToken.equalsIgnoreCase(CONSTANTS.DOCB)){
      // add to parse tree / stack
      Compiler.Scanner.getNextToken()
      variableDefine()
      title()
      body()
      if(Compiler.currentToken.equalsIgnoreCase(CONSTANTS.DOCE)){
        //add to parse tree / stack
      }
      else{
        println("SYNTAX ERROR: End tag was expected when '" + Compiler.currentToken + "' was found.")
        System.exit(1)
      }
      if(Compiler.Scanner.hasNextToken()){
        println("SYNTAX ERROR: Tokens found after End tag.")
      }
    }
    else {
      println("SYNTAX ERROR: Begin tag was expected when '" + Compiler.currentToken + "' was found.")
      System.exit(1)
    }
  }

  override def paragraph(): Unit = {
    if(Compiler.currentToken.equalsIgnoreCase(CONSTANTS.PARAB)){
      //add to parse tree / stack
      variableDefine()
      innerText()
      if(Compiler.currentToken.equalsIgnoreCase(CONSTANTS.PARAE)){
        //add to parse tree / stack
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
      //add to parse tree / stack
      innerText()
    }
  }

  override def link(): Unit = {
    if(Compiler.currentToken.equalsIgnoreCase(CONSTANTS.LINKB)){
      //add to parse tree / stack
      reqText()
      if(Compiler.currentToken.equalsIgnoreCase(CONSTANTS.BRACKETE)){
        //add to parse tree
        if(Compiler.currentToken.equalsIgnoreCase(CONSTANTS.ADDRESSB)){
          //add to parse tree / stack
          reqText()
          if(Compiler.currentToken.equalsIgnoreCase(CONSTANTS.ADDRESSE)){
              //add to parse tree / stack
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
      //add to parse tree / stack
      //add text
      if(Compiler.currentToken.equalsIgnoreCase(CONSTANTS.BOLD)){
        //add to parse tree / stack
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
      //add to parse tree / stack
    }
  }

  override def title(): Unit = {
    if(Compiler.currentToken.equalsIgnoreCase(CONSTANTS.TITLEB)){
      //add to parse tree / stack
      reqText()
      if(Compiler.currentToken.equalsIgnoreCase(CONSTANTS.BRACKETE)){
        //add to parse tree / stack
      }
      else{
        println("SYNTAX ERROR: Closing Title tag was expected")
      }
    }
    else{
      println("SYNTAX ERROR: Title tag was expected")
    }
  }

  override def variableDefine(): Unit = ???

  override def image(): Unit = ???

  override def variableUse(): Unit = ???

  override def heading(): Unit = ???

  override def listItem(): Unit = ???

  def reqText(): Unit = {
    while(CONSTANTS.validText.exists(x => x.equalsIgnoreCase(Compiler.currentToken))){
      //add to parse tree / stack
    }
  }

  def getText(): Unit = {
    while(CONSTANTS.validText.exists(x => x.equalsIgnoreCase(Compiler.currentToken))){
      //add to parse tree / stack
    }
  }

  def parseTree(): Unit = {
    stack.push(Compiler.currentToken)

    if(Compiler.Scanner.hasNextToken()){
      Compiler.Scanner.getNextToken()
    }
  }
}