package edu.towson.cis.cosc455.jchen21.project1

/**
  * Created by jchen21 on 10/11/2017.
  */
class MySyntaxAnalyzer extends SyntaxAnalyzer{
  override def gittex(): Unit = {
    if (Compiler.currentToken.equalsIgnoreCase(CONSTANTS.DOCB)){
      // add to parsetree / stack
      Compiler.Scanner.getNextToken()
    }
    else {
      println("Error")
      System.out(1)
    }
  }

  override def title(): Unit = ???

  override def body(): Unit = ???

  override def paragraph(): Unit = ???

  override def heading(): Unit = ???

  override def variableDefine(): Unit = ???

  override def variableUse(): Unit = ???

  override def bold(): Unit = ???

  override def listItem(): Unit = ???

  override def link(): Unit = ???

  override def image(): Unit = ???

  override def newline(): Unit = ???
}
