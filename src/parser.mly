%{

(* --- Header --- *)

        open Types;;
   
%}

/* ocamlyacc declarations */

%token <string> STRING  
%token <string> VARIABLE
%token <string> NAME
%token <float> UNSIGNEDFLOAT
%token <int> UNSIGNEDINTEGER
%token <float> SIGNEDFLOAT
%token <int> SIGNEDINTEGER

%token DOT
%token COLONHYPHEN
%token ARROW 
%token NOT
%token TERM_UNIFY IS
       ARITH_LESS ARITH_GREATER

%token PLUS MINUS
%token MULT DIV   
       
%token SEMICOLON COMMA COLON
%token UMINUS UPLUS

%token CUT
%token LPAREN RPAREN LBRACKET RBRACKET PIPE
%token EOF

%right SEMICOLON COMMA
%left PLUS MINUS
%left MULT DIV 
%left TERM_UNIFY
%left ARITH_LESS ARITH_GREATER

%left IS 

%right REM MOD
%nonassoc COLONHYPHEN PIPE
%nonassoc DOT  COLON CUT

%type <Types.clause list> program
%start program

%type <Types.term> query
%start query

/* grammar rules */

%%

program: 
    | clause DOT program 
    {
        $1 :: $3 
    }
    
    | EOF
    {
        []
    }
;

query:
    | atomic_formula DOT 
    { 
        $1 
    }
;

clause:
    | fact {$1}
    | rule {$1}
;

rule: head COLONHYPHEN body 
    { 
        Types.ClauseImplication ($1, $3)
    }
;

fact  : head
    { 
        Types.SingleClause $1 
    }
;

head:
    | atomic_formula
    {
        $1 
    }
;

/* we let only boolean operators appear in body part */

body:
    | atomic_formula SEMICOLON body 
    { 
        (* Types.TermOr ($1, $3) *)
        Node("TermOr",[$1;$3])
    }
    | atomic_formula COMMA body 
    { 
        Node("TermAnd",[$1;$3]) 
    }
    | atomic_formula 
    { 
        $1 
    }
;

atomic_formula:
    | term
    { 
        $1 
    }
;

term:
    | term0 { $1 }
;

term0:
    | term1 { $1 }
;

term1:
    | term2 { $1 }
;

term2:
    
    | term3 
    { 
        $1 
    }
;

term3: 
    | term4 
    { 
        $1 
    }
;

term4:
    | NOT term5 
    { 
        (*Types.TermNegation $2*)
        Node("TermNegation",[$2])
    }
    | term5 
    { 
        $1 
    }
;

term5:
    
    | term5 TERM_UNIFY term5
    {
        (*Types.TermTermUnify ($1, $3)*)
        Node("TermTermUnify",[$1;$3])
    }
    
   | term5 IS term5 
    {
        (*Types.TermIs ($1, $3)*) 
         Node("TermTermIs",[$1;$3])
    }
    
    | term5 ARITH_LESS term5
    {
        (*Types.TermArithmeticLess ($1, $3)*)
         Node("TermArithmeticLess",[$1;$3]
 )   }
    | term5 ARITH_GREATER term5
    {
        (*Types.TermArithmeticGreater ($1, $3)*)
         Node("TermArithmeticGreater",[$1;$3])
    }
    
    | term6 
    { 
        $1 
    }
;

term6:
    
    | term7 { $1 }
;

term7:
    | term7 PLUS term7
    {
        (*Types.TermArithmeticPlus ($1, $3)*)
          Node("TermArithmeticPlus",[$1;$3])
    }
    | term7 MINUS term7
    {
        (*Types.TermArithmeticMinus ($1, $3)*)
         Node("TermArithmeticMinus",[$1;$3])
    } 
    | term8 { $1 }
;

term8:
    
    
    | term8 DIV term8
    {
        (*Types.TermArithmeticDiv ($1, $3)*)
         Node("TermArithmeticDiv",[$1;$3])
    }
    
    | term8 MULT term8
    {
        (*Types.TermArithmeticMult ($1, $3)*)
         Node("TermArithmeticMult",[$1;$3])
    }
    
    | term9 { $1 }
;

term9:
    | CUT 
    { 
            (*Types.TermCut *)
             Node("TermCut",[])
    }
    | term10 
    {
        $1 
    }
;

term10:
    | list_term
    {
        $1
    }
    | LPAREN body RPAREN 
    { 
        $2 
    }
    | STRING 
    { 
        (*Types.TermString $1 *)
         Node("TermString",[Node($1,[])])
    }
    | constant 
    { 
        (*Types.TermConstant $1 *)
        Node("TermConstant",[$1])
    }
    | VARIABLE 
    { 
        (*Types.TermVariable $1 *)
         V($1)
    }
    | functor_name LPAREN arguments RPAREN 
    { 
        (*Types.TermFunctor ($1, $3)*) 
        Node("TermFunctor",[$1;$3])
    }
;

list_term:
    | LBRACKET RBRACKET 
    {
        (*Types.TermList (Types.EmptyList)*)
        Node("TermList",[Node("EmptyList",[])])
    }
    | LBRACKET list_body RBRACKET
    {
        Node("TermList",[$2])
        (*Types.TermList $2*)
    }
;

list_body:
    | arguments
    {
        Node("TermNormalList",[$1])
        (*Types.NormalList $1*)
    }
    | term0 PIPE term
    {
        Node("TermDividedList",[$1;$3])
        (*Types.DividedList ($1, $3)*)
    }
    ;

functor_name:
    | name 
    { 
        $1
    } 
;

arguments: 
    | term0 COMMA arguments 
    { 
       (* $1::$3*)
       Node("List",[$1;$3])
    }
    | term0 
    { 
        Node("Tail",[$1]);
    }
;

constant:
    | name 
    { 
        (*ConstantAtom $1 *)
        $1
    }
    | number 
    {
        (*ConstantNumber $1*) 
        $1
    }
;

name:
    | NAME 
    { 
        Node($1,[]) 
    }
;

number:
    
    | UNSIGNEDINTEGER
    {
        (*Types.Integer ($1)*)
        Node((string_of_int $1),[])

    }
   
    | MINUS UNSIGNEDINTEGER %prec UMINUS
    { 
        Node((string_of_int $2),[])
        (*Types.Integer (-$2)*) 

    }
    
    | PLUS UNSIGNEDINTEGER %prec UPLUS
    {
        Node((string_of_int $2),[])
        (*Types.Integer ($2)*)
    }
%%  


(* --- Trailer --- *)

 

