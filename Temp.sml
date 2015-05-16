CM.make "sources.cm";

Control.Print.printDepth := 10000;
Control.Print.printLength := 10000;

fun parse x = Calc.parse_string(x);

fun interpret (x) = assignment4.InterpreterDriver(parse x);
fun printerpret (x) = assignment4.printdriver(parse x);

(*
fun str_load x = assignment4.load (parse x);

fun clear () = assignment4.clearload ();

fun query (s:string) = assignment4.queryit (parse s);

fun run_loaded () = assignment4.interpret_loaded ();

fun prun_loaded () = assignment4.pinterpret_loaded ();
*)


val prog1 = "$ edge(a,b). $edge(a,c). $ edge(a,d). ? edge(a,T). ;";

val prog2 = "$ edge(a,b). $ edge(b,c). $ edge(a,d). $ edge(d,harman(i,j)). $ path(A,C) :- edge(A,B) , edge(B,C).  ? cycl() ;";

val prog3 = "$ member(X,[X|R]). $ member (A,[B|R]) :- member (A,R). ? member(Z , [a,b,c,d]). ;";

val prog4 = "$ append([] , X , X). $ append(Z , [] , Z) . $ append([A|B] , C , [A|Y]) :- append(B,C,Y). ? append( [a] , [b] , [a,b,c]). ;";

val prog5 = "$ edge(a,c). $edge(c,b). $ edge(a,d). $ edge(d,b). ? edge(a,X) , edge(X,b). ;";

val prog6 = "$ edge(a,b). $ edge(b,c). $ edge(a,d). $ edge(d,f). $ path(A,C) :- edge(A,B) , path(B,C). ? path(a,R). ;";

val prog7 = "$ edge(a,b). $ edge(b,c). $ path(X,X). $ path(X,Y):- edge(X,Z),path(Z,Y). ?path(a,M). ? path(b,K). ?path(a,c). ?path(a,v). ;";

val prog8 = "$ edge(a,b). $ edge(a,g). $ edge(b,d). $ edge(c,d). $ edge(g,c). $ edge(g,f). $ edge(c,e). $ edge(e,d). $ edge(z,y). $ path(X,X). $ path(S,T) :- edge(S,T). $ path(D,F) :- edge(D,E) , path(E,F). $ cycle(A,M) :- path(A,M) , path(M,A). ? cycle(d,f).; ";

val prog9 = " $ male(rama). $ male(dash). $ female(sita). $ sibling(rama,laxman). $ sibling(kush,luv). $ mother(sita,kush). $ father ( rama, luv ). $ father ( dash, rama ). $ father ( rama, kush ). $ father ( king, dash ). $ mother ( sita, luv ). $ mother ( sita, kush ). $ uncle(luv,laxman). $ fatherinlaw ( X, Y ) :- mother( Y, Z), father ( W, Z ), father ( X, W ). $ uncle(kush,laxman).  $ gfather ( X, Y ) :- father ( X, Z ), father ( Z, Y ). $ uncle(C,D) :- male(D) , sibling(D,E) , father(C,E). $ uncle(C,D) :- male(D) , sibling(D,E) , mother(C,E). ? sibling(kush,X). ? uncle (S,laxman). ;"

val prog10 = " $"



(*)

val t1 = str_load ( prog8);

val t2 = prun_loaded();

*)

val par1 = parse prog9;

val in1 = interpret prog9;

val praytogod = printerpret prog9;

(*)
val Absyn.PROGRAM(Absyn.GOAL(x1)::xs) = x;

val y = Calc.parse_string "$ edge([a | harman(b,c)] ,d). ? edge(a,x). ;";

val Absyn.PROGRAM(Absyn.FACT(y1)::ys) = y;

val z = unif.mostgenunif(hd(x1),y1);

val tester = unif.mostgenunif(Absyn.NODE("list" , [Absyn.NONVARIABLE( Absyn.LCID "a") , Absyn.VARIABLE("Y")]) , Absyn.NODE("list" , [Absyn.NONVARIABLE( Absyn.LCID "a") , Absyn.NONVARIABLE( Absyn.LCID "b") ]))

*)




