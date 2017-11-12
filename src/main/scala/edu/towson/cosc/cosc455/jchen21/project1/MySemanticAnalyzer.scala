package edu.towson.cosc.cosc455.jchen21.project1

import java.awt.Desktop

import scala.collection.mutable.Stack
import java.io._

class MySemanticAnalyzer {
  var html: String = ""
  var currentToken : String = ""
  var stack = Compiler.Parser.stack
  var convertStack = Stack[String]()
  var outputStack = Stack[String]()
  var tempStack = Stack[String]()
  var boldTag: Int = 0

  def toHTML(): Unit = {
    stack = stack.reverse
    do{
      currentToken = stack.pop()
      if (currentToken.equalsIgnoreCase(CONSTANTS.DOCB)) convertStack.push("<html>\n")
      else if(currentToken.equalsIgnoreCase(CONSTANTS.TITLEB)) title()
      else if(currentToken.equalsIgnoreCase(CONSTANTS.HEADING)) heading()
      //else if(currentToken.equalsIgnoreCase(CONSTANTS.DEFB))
      else if(currentToken.equalsIgnoreCase(CONSTANTS.PARAB)) convertStack.push("<p>")
      else if(currentToken.equalsIgnoreCase(CONSTANTS.PARAE)) convertStack.push("</p>\n")
      else if(currentToken.equalsIgnoreCase(CONSTANTS.BOLD)) bold()
      else if (currentToken.equalsIgnoreCase(CONSTANTS.LISTITEM)) list()
      else if(currentToken.equalsIgnoreCase(CONSTANTS.LINKB)) link()
      else if(currentToken.equalsIgnoreCase(CONSTANTS.IMAGEB)) image()
      else if(currentToken.equalsIgnoreCase(CONSTANTS.NEWLINE)) convertStack.push("<br>\n")
      else if(currentToken.equalsIgnoreCase(CONSTANTS.DOCE))convertStack.push("\n</html>")
    } while (stack.nonEmpty)
while(convertStack.nonEmpty) {
  outputStack.push(convertStack.pop())
}
    html = outputStack.mkString
    println(html)
    outputToFile()
    openHTMLFileInBrowser(Compiler.fileName + ".html")
  }

  def title(): Unit = {
    var title: String = ""
    while(!stack.top.equals("]")){
      tempStack.push(stack.pop())
    }
    while(tempStack.nonEmpty){
      title = tempStack.pop() + title
    }
    convertStack.push("<head>\n\t<title>" + title + "</title>\n</head>\n")
  }

  def heading():Unit = {
    var heading: String = ""
    currentToken = stack.pop()
    while(!CONSTANTS.Keyword.contains(currentToken)){
      tempStack.push(currentToken + " ")
      currentToken = stack.pop()
    }
    while(tempStack.nonEmpty){
      heading = tempStack.pop() + heading
    }
    convertStack.push("<h1>" + heading + "</h1>\n")
  }

  def bold(): Unit = {
    if(boldTag == 0){
      convertStack.push("<b>")
      boldTag = 1
    }
    else{
      convertStack.push("</b>")
      boldTag = 0
    }
  }

  def list(): Unit = {
    var listItem: String = ""
    currentToken = stack.pop()
    while(!CONSTANTS.Keyword.contains(currentToken)){
      tempStack.push(currentToken + " ")
      currentToken = stack.pop()
    }
    while(tempStack.nonEmpty){
      listItem = tempStack.pop() + listItem
    }
    convertStack.push("<li>" + listItem + "</li>\n")
  }

  def link(): Unit = {
    var linkAddress: String = ""
    var linkText: String = ""
    var fullLinkTag: String = ""

    while(!stack.top.equals("]")){
      tempStack.push(stack.pop())
    }
    while(!stack.top.equals("(")){
      stack.pop()
    }
    stack.pop()
    while(tempStack.nonEmpty){
      linkText = tempStack.pop() + linkText
    }
    while(!stack.top.equals(")")){
      tempStack.push(stack.pop())
    }
    while(tempStack.nonEmpty){
      linkAddress = tempStack.pop() + linkAddress
    }
    fullLinkTag = "<a href=\"" + linkAddress + "\">" + linkText + "</a>"
    convertStack.push(fullLinkTag)
  }

  def image(): Unit = {
    var imageAddress: String = ""
    var imageText: String = ""
    var fullImageTag: String = ""

    while(!stack.top.equals("]")){
      tempStack.push(stack.pop())
    }
    while(!stack.top.equals("(")){
      stack.pop()
    }
    stack.pop()
    while(tempStack.nonEmpty){
      imageText = tempStack.pop() + imageText
    }
    while(!stack.top.equals(")")){
      tempStack.push(stack.pop())
    }
    while(tempStack.nonEmpty){
      imageAddress = tempStack.pop() + imageAddress
    }
    fullImageTag = "<img src=\"" + imageAddress + "\" alt=\"" + imageText + "\">"
    convertStack.push(fullImageTag)
  }

  def outputToFile():Unit = {
    val file = new File(Compiler.fileName + ".html")
    val bw = new BufferedWriter(new FileWriter(file))
    bw.write(html)
    bw.close()
  }

  /* * Hack Scala/Java function to take a String filename and open in default web browswer. */
  def openHTMLFileInBrowser(htmlFileStr : String) = {
    val file : File = new File(htmlFileStr.trim)
    println(file.getAbsolutePath)
    if (!file.exists())
      sys.error("File " + htmlFileStr + " does not exist.")

    try {
      Desktop.getDesktop.browse(file.toURI)
    }
    catch {
      case ioe: IOException => sys.error("Failed to open file:  " + htmlFileStr)
      case e: Exception => sys.error("He's dead, Jim!")
    }
  }
}
