(*
 * Control.Print.printDepth := 100;
 *)

fun print_error (s,i,_) =
  TextIO.output(TextIO.stdOut,
    "Error, line " ^ (Int.toString i) ^ ", " ^ s ^ "\n")

structure Calc = struct

  structure CalcLrVals =
    CalcLrValsFun(structure Token = LrParser.Token)

  structure CalcLex =
    CalcLexFun(structure Tokens = CalcLrVals.Tokens)

  structure CalcParser =
    Join(structure LrParser = LrParser
   structure ParserData = CalcLrVals.ParserData
   structure Lex = CalcLex)

  fun invoke lexstream =
       CalcParser.parse(0,lexstream,print_error,())

  fun parse_string s = 
    let val lexer = CalcParser.makeLexer (fn _ => s)
    val dummySEMI = CalcLrVals.Tokens.SEMI(0,0)
    val (result,lexer) = invoke lexer
    in result
  end
end
