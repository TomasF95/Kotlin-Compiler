{
module Lexer where
}

%wrapper "basic"
$alpha = [a-zA-Z]
$digit = [0-9]

tokens :-

-- primitive types
Int                                     { \_ -> INT }
Long                                    { \_ -> LONG }
Float                                   { \_ -> FLOAT }
Double                                  { \_ -> DOUBLE }
Boolean                                 { \_ -> BOOLEAN }
Char                                    { \_ -> CHAR }
String                                  { \_ -> STRING }

-- delimitation characters
\(                                      { \_ -> OPENPAREN }
\)                                      { \_ -> CLOSEPAREN }
\{                                      { \_ -> OPENBRACE }
\}                                      { \_ -> CLOSEBRACE }
\;                                      { \_ -> SEMICOLON }
\:                                      { \_ -> COLON }


-- arithmetic operators
\+                                      { \_ -> PLUS }
\*                                      { \_ -> MULTIPLY }
\-                                      { \_ -> MINUS }
\/                                      { \_ -> DIVIDE }
\%                                      { \_ -> MOD }
-- increment operators

\+\+                                    { \_ -> INCREMENT }
\-\-                                    { \_ -> DECREMENT }

-- self operators
\=                                      { \_ -> ASSIGN }
\+\=                                    { \_ -> SELFPLUS }
\-\=                                    { \_ -> SELFMINUS }
\*\=                                    { \_ -> SELFMULTIPLY }
\/\=                                    { \_ -> SELFDIVIDE }
\%\=                                    { \_ -> SELFMOD }

-- comparison operators
\<                                      { \_ -> LESSER }
\>                                      { \_ -> GREATER }
\<\=                                    { \_ -> LESSEROREQUAL }
\>\=                                    { \_ -> GREATEROREQUAL }

-- equality operators
\=\=                                    { \_ -> EQUALS }
\!\=                                    { \_ -> NOTEQUAL }

-- boolean values
true                                    { \_ -> TRUE }
false                                   { \_ -> FALSE }

-- keywords
if                                      { \_ -> IF }
else                                    { \_ -> ELSE }
while                                   { \_ -> WHILE }

-- logical operators
\&\&                                    { \_ -> AND }
\|\|                                    { \_ -> OR }
\!                                      { \_ -> NOT }




-- variables and functions
val                                     { \_ -> IMMUTABLE }
var                                     { \_ -> VARIABLE }
fun                                     { \_ -> FUNCTION }
main                                    { \_ -> MAIN }
print                                   { \_ -> PRINT }
readln                                  { \_ -> READLINE }


-- identifiers and literals
( $alpha | _ ) ( $alpha | $digit | _ )*   { \s -> ID s }
$digit+                                   { \s -> NUM (read s) }
$digit* \. $digit+                        { \s -> REAL (read s) }
\' \\? . \'                               { \s -> ONECHARSTRING (init (tail s)) }
\" ( ~\" | \\\" )* \"                     { \s -> PHRASE (init (tail s)) }

-- white space and comments
$white+                                 ;
\/ \/ .*                                ;
\/ \* ( ~\* | \* ~\/ | \** \n )* \* \/  ;

{
data Token =
    -- delimitation characters 
      OPENPAREN
    | CLOSEPAREN
    | OPENBRACE
    | CLOSEBRACE
    | SEMICOLON
    | COLON

    
    -- arithmetic operators
    | PLUS
    | MINUS
    | MULTIPLY
    | DIVIDE
    | MOD

    -- increment operators
    | INCREMENT
    | DECREMENT

    -- self operators
    | ASSIGN
    | SELFPLUS
    | SELFMINUS
    | SELFMULTIPLY
    | SELFDIVIDE
    | SELFMOD

    -- comparison operators
    | LESSER
    | GREATER
    | LESSEROREQUAL
    | GREATEROREQUAL

    -- logical operators
    | AND
    | OR
    | NOT

    -- equality operators
    | EQUALS
    | NOTEQUAL

    -- primitive types
    | INT
    | LONG
    | FLOAT
    | DOUBLE
    | BOOLEAN
    | CHAR
    | STRING

    -- boolean values
    | TRUE
    | FALSE

    -- reserved words
    | IF
    | ELSE
    | WHILE

    -- variables and functions
    | IMMUTABLE
    | VARIABLE
    | FUNCTION
    | MAIN
    | PRINT
    | READLINE

    -- identifiers and literals
    | ID String
    | NUM Int
    | REAL Double
    | ONECHARSTRING String 
    | PHRASE String

    deriving (Eq, Show)
}