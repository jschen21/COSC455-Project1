package edu.towson.cosc.cosc455.jchen21.project1

object Compiler{

  var currentToken : String = ""
  var fileContents : String = ""

  val Scanner = new MyLexicalAnalyzer
  val Parser = new MySyntaxAnalyzer
  val SemanticAnalyzer = new MySemanticAnalyzer
  var fileName: String = ""

  def main(args: Array[String]): Unit = {
    checkFile(args)
    readFile(args(0))

    fileName = args(0).substring(0, args(0).length - 4)

    Scanner.getNextToken()
    Parser.gittex()
    SemanticAnalyzer.toHTML()
  }

  def readFile(file : String) = {
    val source = scala.io.Source.fromFile(file)
    fileContents = try source.mkString finally source.close()
  }

  def checkFile(args : Array[String]) = {
    if (args.length != 1) {
      println("USAGE ERROR: wrong number of args fool!")
      System.exit(1)
    }
    else if (! args(0).endsWith(".gtx")) {
      println("USAGE ERROR: wrong extension fool!")
      System.exit(1)
    }
  }
}
