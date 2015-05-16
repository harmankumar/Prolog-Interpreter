structure Tokens = Tokens

type pos = int
type svalue = Tokens.svalue
type ('a,'b) token = ('a,'b) Tokens.token
type lexresult= (svalue,pos) token

val pos = ref 1
fun eof () = Tokens.EOF(!pos,!pos)
	
fun error (e,l,_) = TextIO.output (TextIO.stdOut, String.concat[
	"line ", (Int.toString l), ": ", e, "\n"])


fun inc x = x:=(!x)+1 ; 

%%

%header (functor CalcLexFun(structure Tokens: Calc_TOKENS));

numeric = [-]?([1-9][0-9]*|0);
NumErr = [-]*(0[0-9a-zA-Z]+ | [-]+0+);

man = (0|-?[1-9][0-9]*)\.[0-9]+ ;

ord = (e|E)(-?[1-9][0-9]*|0);

ucid = [A-Z][a-zA-Z0-9_\']*;
lcid = [a-z][a-zA-Z0-9_\']*;

ws = [\ \t];

%%

{ws}+ => (lex());

"\n"	=> (inc(pos);lex());

":-"	=> (Tokens.IF(!pos , !pos));
"."		=> (Tokens.DOT(!pos , !pos));
"("			 => (Tokens.LPAREN(!pos, !pos));
")"    		 => (Tokens.RPAREN(!pos, !pos));
","		 	=> (Tokens.COMMA(!pos, !pos));
";"		=> (Tokens.SEMI(!pos,!pos));


"["		=>(Tokens.LSOLIDPAREN(!pos,!pos));
"]"		=>(Tokens.RSOLIDPAREN(!pos,!pos));
"|" 	=> (Tokens.CONS(!pos,!pos));


"+"         => (Tokens.PLUS(!pos, !pos));
"$"         => (Tokens.IMPOS(!pos,!pos));
"-"         => (Tokens.SUB(!pos, !pos));
"*"         => (Tokens.MULT(!pos, !pos));
"/"         => (Tokens.DIV(!pos, !pos));
"?" 		=> (Tokens.QUERY(!pos,!pos));

"mod"         => (Tokens.MOD(!pos, !pos));
"abs"       => (Tokens.ABS(!pos, !pos));
"**"        => (Tokens.EXP(!pos, !pos));

">"		 	=> (Tokens.GT(!pos, !pos));
"<"			 => (Tokens.LT(!pos, !pos));
"="		 	=> (Tokens.EQ(!pos, !pos));
"<>"	     => (Tokens.NEQ(!pos, !pos));
">="	     => (Tokens.GTEQ(!pos, !pos));
"<="	     => (Tokens.LTEQ(!pos, !pos));


"true" => (Tokens.BOOLEAN( true , !pos , !pos));
"false" => (Tokens.BOOLEAN( false , !pos , !pos));

"True" => (Tokens.BOOLEAN( true , !pos , !pos));
"False" => (Tokens.BOOLEAN( false , !pos , !pos));

{numeric} => (Tokens.NUM(Option.valOf( Int.fromString(yytext)) , !pos , !pos ) );
{NumErr} => (error(" Invalid number "^yytext , !pos , !pos);lex());

{man} => (Tokens.RATIONAL( (Option.valOf(Real.fromString(yytext))) , !pos, !pos) );
{man}{ord} => (Tokens.RATIONAL( (Option.valOf(Real.fromString(yytext))) , !pos, !pos) );

"&&" => (Tokens.AND(!pos,!pos)); 
"||"  => (Tokens.OR(!pos,!pos));
"not"	  => (Tokens.NOT(!pos,!pos));


"Myfun"		=> (Tokens.FUNC(!pos,!pos));


{ucid} => (Tokens.UCID( yytext , !pos , !pos));
{lcid} => (Tokens.LCID( yytext , !pos , !pos));

.	=> (error(" Illegal character "^yytext , !pos , !pos );lex());
