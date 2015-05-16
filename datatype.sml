(* signature ABSYN =
  sig
    datatype expression 	= VARIABLE of string | NUM of int | BOOL of bool | RATIONAL of real
    						| UNARY_OP of string*expression | BINARY_OP of string*expression*expression | NONVARIABLE of string
    		and term 		= TERM of expression | TERM_LIST of (term list) | FUNCTION of expression*(term list)
    		and predicate 	= PREDICATE of expression*(term list)
    		and body 		= BODY of (predicate list)
    		and head 		= HEAD of predicate
    		and declaration	= FACT of predicate | RULE of (head*body) | GOAL of predicate list
    		and program		= PROGRAM of (declaration list)
  end

structure Absyn :> ABSYN =
   struct
    datatype expression 	= VARIABLE of string | NUM of int | BOOL of bool | RATIONAL of real 
    						| UNARY_OP of string*expression | BINARY_OP of string*expression*expression | NONVARIABLE of string
    		and term 		= TERM of expression | TERM_LIST of (term list) | FUNCTION of expression*(term list)
    		and predicate 	= PREDICATE of expression*(term list)
    		and body 		= BODY of (predicate list)
    		and head 		= HEAD of predicate
    		and declaration	= FACT of predicate | RULE of (head*body) | GOAL of predicate list
    		and program		= PROGRAM of (declaration list)
   end
 *)


   signature ABSYN =
  sig
        datatype const = INT of int
        	| BOOL of bool
        	| RATIONAL of real 
        	| LCID of string  
    		and optree = VARIABLE of string 
			| NONVARIABLE of const
        	| NODE of string*(optree list)
    		and declaration	= FACT of optree | RULE of optree * ( optree list ) | GOAL of optree list
    		and program		= PROGRAM of (declaration list)
  end

structure Absyn :> ABSYN =
   struct
        datatype const = INT of int
        	| BOOL of bool
        	| RATIONAL of real 
        	| LCID of string  
    		and optree = VARIABLE of string 
			| NONVARIABLE of const
        	| NODE of string*(optree list)
    		and declaration	= FACT of optree | RULE of optree * ( optree list ) | GOAL of optree list
    		and program		= PROGRAM of (declaration list)
   end

