package edu.towson.cosc.cosc455.jchen21.project1

import java.awt.Desktop

import scala.collection.mutable.Stack
import java.io._

class MySemanticAnalyzer {
  var html: String = ""
  var currentToken : String = ""
  var stack = Compiler.Parser.stack
  var convertStack = Stack[String]() //stack used to convert gittex to HTML
  var outputStack = Stack[String]() //finalized HTML stack
  var tempStack = Stack[String]() //temporary stack that is used to hold items from the Syntal Anlayzer stack
  var variableStack = Stack[String]() //stack used to store block indicators and variables for scoping purposes
  var variableTemp = Stack[String]() //temporary stack that holds onto the items that are popped off of the variable stack
  var boldTag: Int = 0

  def toHTML(): Unit = {
    stack = stack.reverse //reverses the stack to make it easier to read
    while (stack.nonEmpty){
      currentToken = stack.pop() //pops one token at a time from the stack to see what category it belongs to
      if (currentToken.equalsIgnoreCase(CONSTANTS.DOCB)) {
        variableStack.push(currentToken)   //pushes the tag into the variable stack for scoping purposes
        convertStack.push("<html>\n")
      }
      else if(currentToken.equalsIgnoreCase(CONSTANTS.TITLEB)) title()
      else if(currentToken.equalsIgnoreCase(CONSTANTS.HEADING)) heading()
      else if(currentToken.equalsIgnoreCase(CONSTANTS.DEFB)) variableDef()
      else if(currentToken.equalsIgnoreCase(CONSTANTS.USEB)) variableUse()
      else if(currentToken.equalsIgnoreCase(CONSTANTS.PARAB)){
        variableStack.push(currentToken) //pushes the tag into the variable stack for scoping purposes
        convertStack.push("\n<p>")
      }
      else if(currentToken.equalsIgnoreCase(CONSTANTS.PARAE)){
        variableStack.push(currentToken) //pushes the tag into the variable stack for scoping purposes
        convertStack.push("</p>\n")
      }
      else if(currentToken.equalsIgnoreCase(CONSTANTS.BOLD)) bold()
      else if (currentToken.equalsIgnoreCase(CONSTANTS.LISTITEM)) list()
      else if(currentToken.equalsIgnoreCase(CONSTANTS.LINKB)) link()
      else if(currentToken.equalsIgnoreCase(CONSTANTS.IMAGEB)) image()
      else if(currentToken.equalsIgnoreCase(CONSTANTS.NEWLINE)) convertStack.push("<br>\n")
      else if(currentToken.equalsIgnoreCase(CONSTANTS.DOCE)){
        variableStack.push(currentToken)
        convertStack.push("\n</html>")
      }
      else convertStack.push(currentToken)
    }
while(convertStack.nonEmpty) {
  outputStack.push(convertStack.pop() + " ")
}
    html = outputStack.mkString //converts the output stack into a string
    outputToFile() //writes the string into a file
    openHTMLFileInBrowser(Compiler.fileName + ".html")
  }

  def title(): Unit = {
    var title: String = ""
    while(!stack.top.equals("]")){ //iterates until it reaches the closing tag
      tempStack.push(stack.pop())
    }
    stack.pop() //removes the closing tag
    while(tempStack.nonEmpty){
      title = tempStack.pop() + title //transfers the title from a stack to a string
    }
    convertStack.push("<head>\n\t<title>" + title + "</title>\n</head>\n")
  }

  def heading():Unit = {
    var heading: String = ""
    currentToken = stack.pop()
    while(!CONSTANTS.Keyword.contains(currentToken)){ //iterates until it reaches a tag
      tempStack.push(currentToken + " ")
      currentToken = stack.pop()
    }
    while(tempStack.nonEmpty){
      heading = tempStack.pop() + heading
    }
    convertStack.push("<h1>" + heading + "</h1>\n")
  }

  //pushes the variable name and definition to the variable stack
  def variableDef(): Unit = {
    variableStack.push(stack.pop())
    stack.pop() //removes the equal sign
    variableStack.push(stack.pop())
    stack.pop() //removes the closing bracket
  }

  def variableUse(): Unit ={
    val variableName = stack.pop() //saves the variable name
    convertStack.push(findVar(variableName))
    stack.pop()
  }

  //finds the variable definition by popping from the variable stack until the variable name is found
  def findVar(varName : String): String = {
    var varDef : String = ""
    var inScope = true
    var found = false
    while(!found && variableStack.nonEmpty){
      if(variableStack.top.equalsIgnoreCase(CONSTANTS.PARAE)) inScope = false //sets the scope to false when it leaves the current block
      if(variableStack.top.equalsIgnoreCase(CONSTANTS.PARAB)) inScope = true //sets it back to true hen it returns to its current block
      if(variableStack.top.equalsIgnoreCase(varName) && inScope){ //checks the top of the variable stack for the name
        varDef = variableTemp.top   //variable definition would be on top of the variable temp stack
        found = true
      }
      else variableTemp.push(variableStack.pop()) //puts everything from the temp stack back into the variable stack
    }
    if(varDef.equals("")) { //checks if the variable was defined
      println("SEMANTIC ERROR: Variable '" + varName + "' is not defined")
      System.exit(1)
    }

    while(variableTemp.nonEmpty) variableStack.push(variableTemp.pop())

    return varDef
  }

  //sets the bold tags
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
    if(currentToken.equalsIgnoreCase(CONSTANTS.BOLD)) { //checks if there is a bold in the list
      tempStack.push("<b>")
      currentToken = stack.pop()
      while(!currentToken.equalsIgnoreCase(CONSTANTS.BOLD)){
        tempStack.push(currentToken + " ")
        currentToken = stack.pop()
      }
      tempStack.push("</b>")
    }
    while (currentToken.equalsIgnoreCase(CONSTANTS.USEB)) { //checks if there is variable use in the list
      tempStack.push(findVar(stack.pop()) + " ")
      stack.pop()
      currentToken = stack.pop()
    }
    while(tempStack.nonEmpty){
      listItem = tempStack.pop() + listItem
    }
    convertStack.push("<li>" + listItem + "</li>\n")
    if(currentToken.equalsIgnoreCase(CONSTANTS.LISTITEM)) stack.push(currentToken)
  }

  //converts gittex link tags to HTML tags
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
    stack.pop()
    while(tempStack.nonEmpty){
      linkAddress = tempStack.pop() + linkAddress
    }
    fullLinkTag = "<a href=\"" + linkAddress + "\">" + linkText + "</a>"
    convertStack.push(fullLinkTag)
  }

  //converts gittex image tags to HTML tags
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
    stack.pop()
    while(tempStack.nonEmpty){
      imageAddress = tempStack.pop() + imageAddress
    }
    fullImageTag = "<img src=\"" + imageAddress + "\" alt=\"" + imageText + "\">"
    convertStack.push(fullImageTag)
  }

  //writes the outputted string to a file with the correct file name
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
