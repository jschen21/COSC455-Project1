package edu.towson.cosc.cosc455.jchen21.project1
import java.awt.Desktop
import scala.collection.mutable.Stack
import java.io.{File, IOException}

class MySemanticAnalyzer {
  var html: String = ""
  var stack = Compiler.Parser.stack
  var currentToken : String = ""
  var convertStack = Stack[String]()

  def convertToHTML(): Unit = {
    while(!stack.isEmpty){
      currentToken = stack.pop()
      gittexToHtml()
    }
    html = convertStack.mkString

    openHTMLFileInBrowser("file.html")
  }

  def gittexToHtml(): Unit = {
    html.toUpperCase() match{
      case CONSTANTS.DOCB =>
      case CONSTANTS.PARAB =>
      case CONSTANTS.PARAE =>
      case CONSTANTS.BRACKETE =>
      case CONSTANTS.ADDRESSE =>
      case CONSTANTS.BOLD =>
      case CONSTANTS.NEWLINE =>
      case CONSTANTS.DOCE =>
      case _ =>
    }
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
