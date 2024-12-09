{
module Parser where
import Lexer   
import Control.Monad.State
}

%name parse
%tokentype { Token }
%error { handleParseError }

%token

-- primitive types
int         { INT }
long        { LONG }
float       { FLOAT }
double      { DOUBLE }
boolean     { BOOLEAN }
char        { CHAR }
string      { STRING } 

-- delimitation characters
'('         { OPENPAREN }
')'         { CLOSEPAREN}
'{'         { OPENBRACE }
'}'         { CLOSEBRACE }
':'         { COLON }
';'         { SEMICOLON }


-- artithmetic operators 
'+'         { PLUS }
'-'         { MINUS }
'*'         { MULTIPLY }
'/'         { DIVIDE }
'%'         { MOD }

-- increment operatos 
'++'        { INCREMENT }
'--'        { DECREMENT }

-- comparasion operators
'<'         { LESSER }
'>'         { GREATER }
'<='        { LESSEROREQUAL }
'>='        { GREATEROREQUAL }
'=='        { EQUALS }
'!='        { NOTEQUAL }

-- self operators
'='         { ASSIGN }
'+='        { SELFPLUS }
'-='        { SELFMINUS }
'*='        { SELFMULTIPLY }
'/='        { SELFDIVIDE }
'%='        { SELFMOD }

-- keywords
if          { IF }
else        { ELSE }
while       { WHILE }

-- boolean values 
true        { TRUE }
false       { FALSE }

-- logical operators 
'&&'        { AND }
'||'        { OR }
'!'         { NOT }

-- variables and functions
val         { IMMUTABLE }
var         { VARIABLE }
fun         { FUNCTION }
main        { MAIN }
print       { PRINT }
readln      { READLINE }

-- indentefiers and literals
id          { ID $$ }
num         { NUM $$ }
real        { REAL $$ }
letter      { ONECHARSTRING $$ }
phrase      { PHRASE $$ }

%%

-- Language Grammar

Program : MainOP { $1 }

MainOP : fun main '(' ')' Scope { $5 }

Clause :    var id ':' PrimitiveType '=' Argument ';'     { VarAttribution $2 $4 $6 }
          | val id ':' PrimitiveType '=' Argument ';'     { ValAttribution $2 $4 $6 }
          | if '(' Argument ')' Scope else Scope          { IFEClause $3 $5 $7 }
          | if '(' Argument ')' Scope                     { IFClause $3 $5 }          
          | print '(' Argument ')' ';'                    { PrintClause $3 }
          | while '(' Argument ')' Scope                  { WhileClause $3 $5 }
          | id '=' Argument ';'                           { Assign $1 $3 }
          | id '++' ';'                                   { Assign $1 (Sum (Id $1) (Num 1)) }
          | id '--' ';'                                   { Assign $1 (Sub (Id $1) (Num 1)) }
          | id '+=' Argument ';'                          { Assign $1 (Sum (Id $1) $3) }
          | id '-=' Argument ';'                          { Assign $1 (Sub (Id $1) $3) }
          | id '*=' Argument ';'                          { Assign $1 (Mult (Id $1) $3) }
          | id '/=' Argument ';'                          { Assign $1 (Div (Id $1) $3) }
          | id '%=' Argument ';'                          { Assign $1 (Modulus (Id $1) $3) }


Scope : '{' Clauses  '}' { Clauses  $2 }
      | '{' '}'            { Clauses  [] }

Clauses  : Clause Clauses  { $1 : $2 }
           | Clause            { [$1] }


Argument : OrOperator { $1 }

NotArgument : AuxArgument  { $1 }
       | '!' NotArgument { Not $2 }

OrOperator : AndOperator                     { $1 }
               | OrOperator '||' AndOperator { Or $1 $3 }

AndOperator : CompareArgument                     { $1 }
               | AndOperator '&&' CompareArgument { And $1 $3 }

AuxArgument : MULTDIVMODARG               { $1 }
          | AuxArgument '+' MULTDIVMODARG { Sum $1 $3 }
          | AuxArgument '-' MULTDIVMODARG { Sub $1 $3 }

MULTDIVMODARG : Component                { $1 }
           | MULTDIVMODARG '*' Component { Mult $1 $3 }
           | MULTDIVMODARG '/' Component { Div $1 $3 }
           | MULTDIVMODARG '%' Component { Modulus $1 $3 }

CompareArgument : NotArgument                    { $1 }
              | CompareArgument '<' NotArgument  { Lesser $1 $3 }
              | CompareArgument '>' NotArgument  { Greater $1 $3 }
              | CompareArgument '!=' NotArgument { NotEquals $1 $3 }
              | CompareArgument '==' NotArgument { Equals $1 $3 }
              | CompareArgument '>=' NotArgument { GreaterEquals $1 $3 }              
              | CompareArgument '<=' NotArgument { LesserEquals $1 $3 }



PrimitiveType : int     { IntegerType }
     | long             { Integer64Type }
     | float            { FloatType }
     | double           { Float64Type }
     | boolean          { BoolType }
     | char             { CharType }
     | string           { StringType }

Component : num            { Num $1 }
     | real                { Real $1 }
     | id                  { Id $1 }
     | true                { BooleanValue True }
     | false               { BooleanValue False }
     | letter              { Char $1 }
     | phrase              { Phrase $1 }
     | '(' Argument ')'    { $2 }
     | readln '(' ')'      { ReadClause }

{
data Program = Scope
  deriving Show

data Scope = Clauses  [Clause]
  deriving Show

data PrimitiveType
  = IntegerType
  | Integer64Type
  | FloatType
  | Float64Type
  | BoolType
  | CharType
  | StringType
  deriving (Eq, Show)


data Clause
  = VarAttribution String PrimitiveType Argument
  | ValAttribution String PrimitiveType Argument
  | Assign String Argument
  | WhileClause Argument Scope
  | PrintClause Argument
  | IFClause Argument Scope
  | IFEClause Argument Scope Scope
  deriving Show

data Argument
  = Num Int
  | ReadClause
  | Real Double
  | Id String
  | BooleanValue Bool
  | Char String 
  | Phrase String
  | Sum Argument Argument
  | Sub Argument Argument
  | Mult Argument Argument
  | Div Argument Argument
  | Modulus Argument Argument
  | Greater Argument Argument
  | GreaterEquals Argument Argument
  | Lesser Argument Argument
  | LesserEquals Argument Argument
  | Equals Argument Argument
  | NotEquals Argument Argument
  | And Argument Argument
  | Or Argument Argument
  | Not Argument
  deriving Show

handleParseError :: [Token] -> a
handleParseError tokens = error (show tokens)
}
