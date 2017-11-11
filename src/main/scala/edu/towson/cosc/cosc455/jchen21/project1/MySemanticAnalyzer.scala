package edu.towson.cosc.cosc455.jchen21.project1
import java.awt.Desktop
import scala.collection.mutable.Stack
import java.io.{File, IOException}

class MySemanticAnalyzer {
  var html: String = ""
  var stack = Compiler.Parser.stack
  var currentToken : String = ""
  var convertStack = Stack[String]()
  var tempStack = Stack[String]()
  var boldTag: Int = 0

  def convertToHTML(): Unit = {
    while(!stack.isEmpty){
      currentToken = stack.pop()
      gittexToHtml()
    }
    println(convertStack.mkString)
    html = convertStack.mkString

  }

  def gittexToHtml(): Unit = {
    if(currentToken.equalsIgnoreCase(CONSTANTS.DOCB)) convertStack.push("<html>\n")
    else if(currentToken.equalsIgnoreCase(CONSTANTS.PARAB)) convertStack.push("<p>")
    else if(currentToken.equalsIgnoreCase(CONSTANTS.PARAE)) convertStack.push("</p>\n")
    else if(currentToken.equalsIgnoreCase(CONSTANTS.BRACKETE)) titleOrVar()
    else if(currentToken.equalsIgnoreCase(CONSTANTS.ADDRESSE)) endOfAddress()
    else if(currentToken.equalsIgnoreCase(CONSTANTS.BOLD)) bold()
    else if(currentToken.equalsIgnoreCase(CONSTANTS.LISTITEM) || currentToken.equalsIgnoreCase(CONSTANTS.HEADING)) headOrList()
    else if(currentToken.equalsIgnoreCase(CONSTANTS.NEWLINE)) convertStack.push("<br>\n")
    else if(currentToken.equalsIgnoreCase(CONSTANTS.DOCE)) convertStack.push("</html>")
    else convertStack.push(currentToken)
  }

  def titleOrVar(): Unit = {
    while(stack.top.charAt(0) != '\\'){
      tempStack.push(stack.pop())
    }
    val tag: String = stack.pop()
    if(tag.equalsIgnoreCase(CONSTANTS.TITLEB)) title()
    else if (tag.equalsIgnoreCase(CONSTANTS.DEFB)) variableDef()
    else if(tag.equalsIgnoreCase(CONSTANTS.USEB)) variableUSe()
  }

  def title(): Unit = {
    var title: String = ""
    while(!tempStack.isEmpty){
      title = title + tempStack.pop
    }
    convertStack.push("<head>\n\t<title>" + title + "</title>\n</head>")
  }
  def variableDef(): Unit = {}
  def variableUSe(): Unit = {
    val varName = tempStack.pop()
    if(!tempStack.isEmpty) tempStack.pop()
  }

  def endOfAddress(): Unit = {
    while(!stack.top.equalsIgnoreCase(CONSTANTS.LINKB) && !stack.top.equalsIgnoreCase(CONSTANTS.IMAGEB)){
      tempStack.push(stack.pop())
    }
    val startTag = stack.pop()
    if(startTag.equalsIgnoreCase(CONSTANTS.LINKB)) link()
    else image()
  }

  def link(): Unit = {
    var linkAddress: String = ""
    var linkText: String = ""
    var fullLinkTag: String = ""

    tempStack = tempStack.reverse
    while(!tempStack.top.equalsIgnoreCase(CONSTANTS.ADDRESSB)){
      linkAddress = tempStack.pop() + linkAddress
    }
    tempStack.pop()
    while(!tempStack.isEmpty){
      if(!tempStack.top.equals("]")) linkText = tempStack.pop() + linkText
      else tempStack.pop()
    }
    fullLinkTag = "<a href=\"" + linkAddress + "\">" + linkText + "</a>"
    convertStack.push(fullLinkTag)
  }
  def image(): Unit = {
    var imageAddress: String = ""
    var imageText: String = ""
    var fullImageTag: String = ""

    tempStack = tempStack.reverse
    while(!tempStack.top.equalsIgnoreCase(CONSTANTS.ADDRESSB)){
      imageAddress = tempStack.pop() + imageAddress
    }
    tempStack.pop()
    while(!tempStack.isEmpty){
      if(!tempStack.top.equals("]")) imageText = tempStack.pop() + imageText
      else tempStack.pop()
    }
    fullImageTag = "<img src=\"" + imageAddress + "\" alt=\"" + imageText + "\">"
    convertStack.push(fullImageTag)
  }

  def bold(): Unit = {
    if(boldTag == 0){
      convertStack.push("</b>")
      boldTag = 1
    }
    else{
      convertStack.push("<b>")
      boldTag = 0
    }
  }

  def headOrList(): Unit = {
    while(!stack.top.equals("#") && !stack.top.equals("+")){
      tempStack.push(stack.pop())
    }
    val startTag: String = stack.pop()
    if(startTag.equals("#")) heading()
    else list()
  }

  def heading(): Unit = {
    var heading: String = ""
    while(!tempStack.isEmpty) heading = heading + tempStack.pop()
    println(heading)
    convertStack.push("<h1>" + heading + "</h1>")
  }

  def list(): Unit = {

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
