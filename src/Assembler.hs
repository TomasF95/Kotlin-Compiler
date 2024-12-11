module Assembler where

import Data.Char (isAlphaNum, isSpace)
import Parser (Clause (..), Argument (..), PrimitiveType (..), Scope (..))
import Generator (Instruction (..), Temp, BinaryOP(..))
import Data.Bool (bool)

assemble :: [Instruction] -> [(Temp, Int)] -> String
assemble i t = "\n.data\n\tbuffer: .space 100\n" ++ genInfo i ++ "\n.text\nmain:\n"++ (assemble' i t) 

assemble' :: [Instruction] -> [(Temp, Int)] -> String
assemble' [] t = "\tli $v0, 10\n\tsyscall\n\nprint_string:\n\tli $v0, 4\n\tsyscall\n\tjr $ra\n\nprint_int:\n\tli $v0, 1\n\tsyscall\n\tjr $ra\n\nprint_float:\n\tli $v0, 2\n\tsyscall\n\tjr $ra\n\nread:\n\tli $v0, 8\n\tla $a0, buffer\n\tli $a1, 100\n\tsyscall\n\tCOPY $a0, $a0\n\tjr $ra\n"
assemble' ((LABEL label):xs) t = label ++ ":\n" ++ assemble' xs t
assemble' ((JUMP label):xs) t = "\t" ++ "j\t" ++ label ++ "\n" ++ assemble' xs t
assemble' ((COND t1 op t2 label1 label2):(LABEL label3):xs) t = assembleCondition (COND t1 op t2 label1 label2) (LABEL label3) t ++ (assemble' xs t)
assemble' ((COND t1 Equal t2 label1 label2):xs) t =  "\t" ++ "beq\t" ++ (fetchAssign t t1) ++ ", " ++ (fetchAssign t t2) ++ ", " ++ label1 ++ "\n\tj\t" ++ label2 ++ "\n" ++ assemble' xs t 
assemble' ((COND t1 Smaller t2 label1 label2):xs) t =  "\t" ++ "blt\t" ++ (fetchAssign t t1) ++ ", " ++ (fetchAssign t t2) ++ ", " ++ label1 ++ "\n\tj\t" ++ label2 ++ "\n" ++ assemble' xs t 
assemble' ((COND t1 Bigger t2 label1 label2):xs) t =  "\t" ++ "bge\t" ++ (fetchAssign t t1) ++ ", " ++ (fetchAssign t t2) ++ ", " ++ label1 ++ "\n\tj\t" ++ label2 ++ "\n" ++ assemble' xs t
assemble' (i:is) t = "\t" ++ (assembleArgument i t) ++ "\n" ++ assemble' is t

assembleCondition :: Instruction -> Instruction -> [(Temp, Int)] -> String
assembleCondition (COND t1 Equal t2 label1 label2) (LABEL label3) t 
    |label1 == label3 = "\tbne\t" ++ (fetchAssign t t1) ++ ", " ++ (fetchAssign t t2) ++ ", " ++ label2 ++ "\n" ++ label1 ++ ":\n"
    |label2 == label3 = "\tbeq\t" ++ (fetchAssign t t1) ++ ", " ++ (fetchAssign t t2) ++ ", " ++ label1 ++ "\n" ++ label2 ++ ":\n"
    |otherwise = "\tbeq\t" ++ (fetchAssign t t1) ++ ", " ++ (fetchAssign t t2) ++ ", " ++ label1 ++ "\n\tj\t" ++ label2 ++ "\n" ++ label3 ++ ":\n"
assembleCondition (COND t1 SmallerEqual t2 label1 label2) (LABEL label3) t 
    |label1 == label3 = "\tbge\t" ++ (fetchAssign t t1) ++ ", " ++ (fetchAssign t t2) ++ ", " ++ label2 ++ "\n" ++ label1 ++ ":\n"
    |label2 == label3 = "\t\tblt\t" ++ (fetchAssign t t1) ++ ", " ++ (fetchAssign t t2) ++ ", " ++ label1 ++ "\n" ++ label2 ++ ":\n"
    |otherwise = "\tblt\t" ++ (fetchAssign t t1) ++ ", " ++ (fetchAssign t t2) ++ ", " ++ label1 ++ "\n\tj\t" ++ label2 ++ "\n" ++ label3 ++ ":\n"
assembleCondition (COND t1 BiggerEqual t2 label1 label2) (LABEL label3) t
    |label1 == label3 = "\tblt\t" ++ (fetchAssign t t1) ++ ", " ++ (fetchAssign t t2) ++ ", " ++ label2 ++ "\n" ++ label1 ++ ":\n"
    |label2 == label3 = "\t\tbge\t" ++ (fetchAssign t t1) ++ ", " ++ (fetchAssign t t2) ++ ", " ++ label1 ++ "\n" ++ label2 ++ ":\n"
    |otherwise = "\tbge\t" ++ (fetchAssign t t1) ++ ", " ++ (fetchAssign t t2) ++ ", " ++ label1 ++ "\n\tj\t" ++ label2 ++ "\n" ++ label3 ++ ":\n"
assembleCondition (COND t1 Smaller t2 label1 label2) (LABEL label3) t
    |label1 == label3 = "\tbge\t" ++ (fetchAssign t t1) ++ ", " ++ (fetchAssign t t2) ++ ", " ++ label2 ++ "\n" ++ label1 ++ ":\n"
    |label2 == label3 = "\t\tblt\t" ++ (fetchAssign t t1) ++ ", " ++ (fetchAssign t t2) ++ ", " ++ label1 ++ "\n" ++ label2 ++ ":\n"
    |otherwise = "\tblt\t" ++ (fetchAssign t t1) ++ ", " ++ (fetchAssign t t2) ++ ", " ++ label1 ++ "\n\tj\t" ++ label2 ++ "\n" ++ label3 ++ ":\n"
assembleCondition (COND t1 Bigger t2 label1 label2) (LABEL label3) t
    |label1 == label3 = "\tblt\t" ++ (fetchAssign t t1) ++ ", " ++ (fetchAssign t t2) ++ ", " ++ label2 ++ "\n" ++ label1 ++ ":\n"
    |label2 == label3 = "\t\tbge\t" ++ (fetchAssign t t1) ++ ", " ++ (fetchAssign t t2) ++ ", " ++ label1 ++ "\n" ++ label2 ++ ":\n"
    |otherwise = "\tbge\t" ++ (fetchAssign t t1) ++ ", " ++ (fetchAssign t t2) ++ ", " ++ label1 ++ "\n\tj\t" ++ label2 ++ "\n" ++ label3 ++ ":\n"

assembleArgument :: Instruction -> [(Temp, Int)] -> String
assembleArgument (OP Addittion t1 t2 t3 IntegerType) t = "add " ++ (fetchAssign t t1) ++ ", " ++ (fetchAssign t t2) ++ ", " ++ (fetchAssign t t3)
assembleArgument (OP Subtraction t1 t2 t3 IntegerType) t = "sub " ++ (fetchAssign t t1) ++ ", " ++ (fetchAssign t t2) ++ ", " ++ (fetchAssign t t3)
assembleArgument (OP Multiplication t1 t2 t3 IntegerType) t = "mul " ++ (fetchAssign t t1) ++ ", " ++ (fetchAssign t t2) ++ ", " ++ (fetchAssign t t3)
assembleArgument (OP Division t1 t2 t3 IntegerType) t = "div " ++ (fetchAssign t t1) ++ ", " ++ (fetchAssign t t2) ++ ", " ++ (fetchAssign t t3)
assembleArgument (OP Rest t1 t2 t3 IntegerType) t = "rem " ++ (fetchAssign t t1) ++ ", " ++ (fetchAssign t t2) ++ ", " ++ (fetchAssign t t3)
assembleArgument (OP Addittion t1 t2 t3 FloatType) t = "add.s " ++ (getFloat t t1) ++ ", " ++ (getFloat t t2) ++ ", " ++ (getFloat t t3)
assembleArgument (OP Subtraction t1 t2 t3 FloatType) t = "sub.s " ++ (getFloat t t1) ++ ", " ++ (getFloat t t2) ++ ", " ++ (getFloat t t3)
assembleArgument (OP Multiplication t1 t2 t3 FloatType) t = "mul.s " ++ (getFloat t t1) ++ ", " ++ (getFloat t t2) ++ ", " ++ (getFloat t t3)
assembleArgument (OP Division t1 t2 t3 FloatType) t = "div.s " ++ (getFloat t t1) ++ ", " ++ (getFloat t t2) ++ ", " ++ (getFloat t t3)
assembleArgument (OP Rest t1 t2 t3 FloatType) t = "rem.s " ++ (getFloat t t1) ++ ", " ++ (getFloat t t2) ++ ", " ++ (getFloat t t3)
assembleArgument (COPYI t1 t2) t = "li " ++ (fetchAssign t t1) ++ ", " ++ show t2
assembleArgument (COPYS t1 s) t = "la " ++ (fetchAssign t t1) ++ ", " ++ (genHash s)
assembleArgument (COPYR t1 f) t = "l.s " ++ (getFloat t t1) ++ ", " ++ (genFloatHash f)
assembleArgument (COPYB t1 True) t = "li " ++ (fetchAssign t t1) ++ ", 1"
assembleArgument (COPYB t1 False) t = "li " ++ (fetchAssign t t1) ++ ", 0"
assembleArgument (PRINT t1 BoolType) t = "move $a0, " ++ (fetchAssign t t1) ++ "\n\tjal print_int"
assembleArgument (PRINT t1 IntegerType) t = "move $a0, " ++ (fetchAssign t t1) ++ "\n\tjal print_int"
assembleArgument (PRINT t1 FloatType) t = "mov.s $f12, " ++ (getFloat t t1) ++ "\n\tjal print_float"
assembleArgument (PRINT t1 StringType) t = "la $a0, " ++ (fetchAssign t t1) ++ "\n\tjal print_string"
assembleArgument (READLN t1) t = "jal read\n\tmove " ++ (fetchAssign t t1) ++ ", $a0"

fetchAssign :: [(Temp, Int)] -> Temp -> String
fetchAssign [] temp = "$t0"
fetchAssign ((t, i):xs) temp
  | t == temp = if i == 33 then "$t" ++ show i else "$s" ++ show i
  | otherwise = fetchAssign xs temp

getFloat :: [(Temp, Int)] -> Temp -> String
getFloat [] temp = "$f0"
getFloat ((t, i):xs) temp
  | t == temp = if i == 33 then dumpFloat t else "$f" ++ show i
    | otherwise = getFloat xs temp

dumpFloat :: Temp -> String
dumpFloat t = "s.s " ++ t ++ ", 0($sp)\n"

dump :: Temp -> String
dump t = "sw " ++ t ++ ", 0($sp)\n"

genHash :: String -> String 
genHash s = "string_" ++ (map (\n -> if isAlphaNum n then n else '_') s)

genFloatHash :: Double -> String
genFloatHash f = "float_" ++ map (\n -> if isAlphaNum n then n else '_') (show f)

genInfo ::  [Instruction] -> String
genInfo [] = ""
genInfo ((COPYS _ s):xs) = "\t" ++ (genHash s) ++ ": .asciiz " ++ s ++ "\n" ++ genInfo xs
genInfo ((COPYR _ f):xs) = "\t" ++ (genFloatHash f) ++ ": .float " ++ show f ++ "\n" ++ genInfo xs

