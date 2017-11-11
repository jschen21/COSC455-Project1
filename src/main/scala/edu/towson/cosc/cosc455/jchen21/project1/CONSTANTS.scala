package edu.towson.cosc.cosc455.jchen21.project1


object CONSTANTS {
  val DOCB : String = 	"\\BEGIN"
  val DOCE : String = 	"\\END"
  val TITLEB : String = "\\TITLE["
  val BRACKETE : String = "]"
  val HEADING : String = "#"
  val PARAB : String = "\\PARAB"
  val PARAE : String = "\\PARAE"
  val BOLD : String = "*"
  val LISTITEM : String = "+"
  val NEWLINE : String = "\\\\"
  val LINKB : String = "["
  val ADDRESSB : String = "("
  val ADDRESSE : String = ")"
  val IMAGEB : String = "!["
  val DEFB : String = "\\DEF["
  val EQSIGN : String = "="
  val USEB : String = "\\USE["

  val letters : List[String] = List("a","b","c","d","e","f","g","h","i","j","k","l","m","n","o","p","q","r","s","t","u","v","w","x","y","z")
  val numbersEtc : List[String] = List("1","2","3","4","5","6","7","8","9","0",",",".","\"",":","?","_","/", "'", "")
  val whiteSpace : List[String] = List(" ", "\t", "\n", "\b","\f","\r")
  val validText : List[String] = whiteSpace ::: letters ::: numbersEtc

  val specialChar : List[String] = List("=","[", "]","#","*", "+", "(", ")")
  val validToken: List[String] = validText ::: specialChar
  val innerText: List[String] = List(USEB, HEADING, BOLD, LISTITEM, IMAGEB, LINKB) ::: validText
  val Keyword : List[String] = List(DOCB, DOCE, TITLEB, BRACKETE, HEADING, PARAB, PARAE, BOLD, LISTITEM, NEWLINE, LINKB, ADDRESSB, ADDRESSE, IMAGEB, DEFB, EQSIGN, USEB)
}