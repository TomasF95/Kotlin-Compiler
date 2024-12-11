module Generator where

import Parser (Clause (..), Argument (..), Scope (..), PrimitiveType (..))
import SymbolTable (SymbolTable, STI (..), find, insertEntry, setupTable)
import Control.Monad.State

data Instruction = COPY Temp Temp PrimitiveType
           | COPYI Temp Int
           | COPYR Temp Double
           | COPYB Temp Bool
           | COPYS Temp String
           | OP BinaryOP Temp Temp Temp PrimitiveType
           | LABEL Label
           | JUMP Label
           | COND Temp BinaryOP Temp Label Label
           | PRINT Temp PrimitiveType
           | READLN Temp 

instance Show Instruction where
    show (COPY t1 t2 _) = "COPY " ++ show t1 ++ " " ++ show t2
    show (COPYI t i) = "COPYI " ++ show t ++ " " ++ show i
    show (COPYR t r) = "COPYR " ++ show t ++ " " ++ show r
    show (COPYB t b) = "COPYB " ++ show t ++ " " ++ show b
    show (COPYS t s) = "COPYS " ++ show t ++ " " ++ show s
    show (LABEL l) = "LABEL " ++ show l
    show (JUMP l) = "JUMP " ++ show l
    show (PRINT t _) = "PRINT " ++ show t
    show (READLN t) = "READLN " ++ show t
    show (COND t1 op t2 l1 l2) = "COND " ++ show t1 ++ " " ++ show op ++ " " ++ show t2 ++ " " ++ show l1 ++ " " ++ show l2
    show (OP op t1 t2 t3 _) = show op ++ " " ++ show t1 ++ " " ++ show t2 ++ " " ++ show t3


data BinaryOP = Addittion
              | Subtraction
              | Multiplication
              | Division
              | Rest
              | Equal
              | Bigger
              | Smaller
              | BiggerEqual
              | SmallerEqual
            deriving Show
data UnaryOP = Negation
            deriving Show

type Temp = String
type Label = String
type Count = (Int, Int)

newTemp :: State Count Temp
newTemp = do
    (temp, label) <- get
    put (temp + 1, label)
    return ("t" ++ show temp)

newLabel :: State Count Label
newLabel = do 
    (temp, label) <- get
    put (temp, label + 1)
    return ("L" ++ show label)

translateArgument :: Argument -> SymbolTable -> Temp -> State Count [Instruction]
translateArgument (Num x) _ xs = return [COPYI xs x]
translateArgument (Real x) _ xs = return [COPYR xs x]
translateArgument (BooleanValue True) _ xs = return [COPYI xs 0]
translateArgument (BooleanValue False) _ xs = return [COPYI xs 1]
translateArgument (Char x) _ xs = return [COPYS xs x]
translateArgument (Phrase x) _ xs = return [COPYS xs x]
translateArgument (ReadClause) _ xs = return [READLN xs]
translateArgument (Parser.Sum arg1 arg2) table xs = binAuxliary Addittion arg1 arg2 table xs 
translateArgument (Parser.Sub arg1 arg2) table xs = binAuxliary Subtraction arg1 arg2 table xs 
translateArgument (Parser.Mult arg1 arg2) table xs = binAuxliary Multiplication arg1 arg2 table xs 
translateArgument (Parser.Div arg1 arg2) table xs = binAuxliary Division arg1 arg2 table xs 
translateArgument (Parser.Modulus arg1 arg2) table xs = binAuxliary Rest arg1 arg2 table xs 
translateArgument (Parser.And arg1 arg2) table xs = condAuxliary (Parser.And arg1 arg2) table xs 
translateArgument (Parser.Or arg1 arg2) table xs = condAuxliary (Parser.Or arg1 arg2) table xs
translateArgument (Parser.Equals arg1 arg2) table xs = condAuxliary (Parser.Equals arg1 arg2) table xs
translateArgument (Parser.NotEquals arg1 arg2) table xs = condAuxliary (Parser.NotEquals arg1 arg2) table xs
translateArgument (Parser.Greater arg1 arg2) table xs = condAuxliary (Parser.Greater arg1 arg2) table xs
translateArgument (Parser.Lesser arg1 arg2) table xs = condAuxliary (Parser.Lesser arg1 arg2) table xs
translateArgument (Parser.GreaterEquals arg1 arg2) table xs = condAuxliary (Parser.GreaterEquals arg1 arg2) table xs
translateArgument (Parser.LesserEquals arg1 arg2) table xs = condAuxliary (Parser.LesserEquals arg1 arg2) table xs
translateArgument (Parser.Not arg) table xs = condAuxliary (Parser.Not arg) table xs
translateArgument (Parser.Id id) table xs = do
    case find id table of
        Just (TEMPVALUE temp t) -> return [COPY xs temp t]
        Just (VARINFO _ _) -> error "wrong type"
        Nothing -> error "Variable not declared"

binAuxliary :: BinaryOP -> Argument -> Argument -> SymbolTable -> Temp -> State Count [Instruction]
binAuxliary op arg1 arg2 table xs = do
    temp1 <- newTemp
    temp2 <- newTemp
    instr1 <- translateArgument arg1 table temp1
    instr2 <- translateArgument arg2 table temp2
    t <- verifyArgumentPrimitiveType arg1 table
    return (instr1 ++ instr2 ++ [OP op xs temp1 temp2 t])

condAuxliary :: Argument -> SymbolTable -> Temp -> State Count [Instruction]
condAuxliary cond table xd = do
    label1 <- newLabel
    label2 <- newLabel
    label3 <- newLabel
    instr <- translateCondition cond table label1 label2
    return (instr ++ [LABEL label1] ++ [COPYI xd 1] ++ [JUMP label3] ++ [LABEL label2] ++ [COPYI xd 0] ++ [LABEL label3])

verifyArgumentPrimitiveType :: Argument -> SymbolTable -> State Count PrimitiveType
verifyArgumentPrimitiveType (Num _) _ = return IntegerType
verifyArgumentPrimitiveType (Real _) _ = return FloatType
verifyArgumentPrimitiveType (BooleanValue _) _ = return BoolType
verifyArgumentPrimitiveType (Char _) _ = return CharType
verifyArgumentPrimitiveType (Phrase _) _ = return StringType
verifyArgumentPrimitiveType (Sum a1 _) table = do
    t1 <- verifyArgumentPrimitiveType a1 table
    return t1
verifyArgumentPrimitiveType (Sub a1 _) table = do
    t1 <- verifyArgumentPrimitiveType a1 table
    return t1
verifyArgumentPrimitiveType (Mult a1 _) table = do
    t1 <- verifyArgumentPrimitiveType a1 table
    return t1
verifyArgumentPrimitiveType (Div a1 _) table = do
    t1 <- verifyArgumentPrimitiveType a1 table
    return t1
verifyArgumentPrimitiveType (Modulus a1 _) table = do
    t1 <- verifyArgumentPrimitiveType a1 table
    return t1
verifyArgumentPrimitiveType (And _ _) table = return BoolType
verifyArgumentPrimitiveType (Or _ _) table = return BoolType
verifyArgumentPrimitiveType (Not _) table = return BoolType
verifyArgumentPrimitiveType (Equals _ _) table = return BoolType
verifyArgumentPrimitiveType (NotEquals _ _) table = return BoolType
verifyArgumentPrimitiveType (Lesser _ _) table = return BoolType
verifyArgumentPrimitiveType (Greater _ _) table = return BoolType
verifyArgumentPrimitiveType (LesserEquals _ _) table = return BoolType
verifyArgumentPrimitiveType (GreaterEquals _ _) table = return BoolType
verifyArgumentPrimitiveType (Id id) table = do
    case find id table of
        Just (TEMPVALUE _ t) -> return t
        Just (VARINFO _ _) -> error "wrong type"
        Nothing -> error "Variable not declared"

translateCondition :: Argument -> SymbolTable -> Label -> Label -> State Count [Instruction]
translateCondition (Parser.Lesser arg1 arg2) table l1 l2 = do 
    temp1 <- newTemp
    temp2 <- newTemp
    instr1 <- translateArgument arg1 table temp1
    instr2 <- translateArgument arg2 table temp2
    return (instr1 ++ instr2 ++ [COND temp1 Smaller temp2 l1 l2])
translateCondition (Parser.Greater arg1 arg2) table l1 l2 = do
    temp1 <- newTemp
    temp2 <- newTemp
    instr1 <- translateArgument arg1 table temp1
    instr2 <- translateArgument arg2 table temp2
    return (instr1 ++ instr2 ++ [COND temp1 Bigger temp2 l1 l2])
translateCondition (Parser.LesserEquals arg1 arg2) table l1 l2 = do
    temp1 <- newTemp
    temp2 <- newTemp
    instr1 <- translateArgument arg1 table temp1
    instr2 <- translateArgument arg2 table temp2
    return (instr1 ++ instr2 ++ [COND temp1 SmallerEqual temp2 l1 l2])
translateCondition (Parser.GreaterEquals arg1 arg2) table l1 l2 = do
    temp1 <- newTemp
    temp2 <- newTemp
    instr1 <- translateArgument arg1 table temp1
    instr2 <- translateArgument arg2 table temp2
    return (instr1 ++ instr2 ++ [COND temp1 BiggerEqual temp2 l1 l2])
translateCondition (Parser.Equals arg1 arg2) table l1 l2 = do
    temp1 <- newTemp
    temp2 <- newTemp
    instr1 <- translateArgument arg1 table temp1
    instr2 <- translateArgument arg2 table temp2
    return (instr1 ++ instr2 ++ [COND temp1 Equal temp2 l1 l2])
translateCondition (Parser.NotEquals arg1 arg2) table l1 l2 = do
    temp1 <- newTemp
    temp2 <- newTemp
    instr1 <- translateArgument arg1 table temp1
    instr2 <- translateArgument arg2 table temp2
    return (instr1 ++ instr2 ++ [COND temp1 Equal temp2 l1 l2])
translateCondition (Parser.And arg1 arg2) table l1 l2 = do
    label <- newLabel
    instr1 <- translateCondition arg1 table label l2
    instr2 <- translateCondition arg2 table l1 l2
    return (instr1 ++ [LABEL label] ++ instr2)
translateCondition (Parser.Or arg1 arg2) table l1 l2 = do
    label <- newLabel
    instr1 <- translateCondition arg1 table l1 label
    instr2 <- translateCondition arg2 table l1 l2
    return (instr1 ++ [LABEL label] ++ instr2)
    
translateCondition (Not arg) table l1 l2 = translateCondition arg table l2 l1
translateCondition (BooleanValue True) _ l1 _ = return [JUMP l1]
translateCondition (BooleanValue False) _ _ l2 = return [JUMP l2]
translateCondition (Id id) table l1 l2 = do
    temptrue <- newTemp
    instrtrue <- translateArgument (BooleanValue True) table temptrue
    case find id table of
        Just (TEMPVALUE temp _) -> return (instrtrue ++ [COND temp Equal temptrue l1 l2])
        Just (VARINFO _ _) -> error "wrong type"
        Nothing -> error "Variable not declared"
translateCondition _ _ _ _ = error "Invalid condition"

translateClause :: Clause -> SymbolTable -> State Count [Instruction]
translateClause (VarAttribution id _ arg) table = do
        case find id table of
            Just (TEMPVALUE temp _) -> translateArgument arg table temp
            Just (VARINFO _ _) -> error "wrong type"
            Nothing -> error "Variable not declared"
translateClause (ValAttribution id _ arg) table = do
        case find id table of
            Just (TEMPVALUE temp _) -> translateArgument arg table temp
            Just (VARINFO _ _) -> error "wrong type"
            Nothing -> error "Variable not declared"
translateClause (Assign id arg) table = do
        case find id table of
            Just (TEMPVALUE xs _) -> translateArgument arg table xs
            Just (VARINFO _ _) -> error "wrong type"
            Nothing -> error "Variable not declared"
translateClause (IFClause cond scope) table = do
        label1 <- newLabel
        label2 <- newLabel
        instr1 <- translateCondition cond table label1 label2
        instr2 <- translateScope scope table
        return (instr1 ++ [LABEL label1] ++ instr2 ++ [LABEL label2])
translateClause (WhileClause cond scope) table = do
        label1 <- newLabel
        label2 <- newLabel
        label3 <- newLabel
        instr1 <- translateCondition cond table label2 label3
        instr2 <- translateScope scope table
        return ([LABEL label1] ++ instr1 ++ [LABEL label2] ++ instr2 ++ [JUMP label1] ++ [LABEL label3])
translateClause (PrintClause arg) table = do
        temp <- newTemp
        instr <- translateArgument arg table temp
        t <- verifyArgumentPrimitiveType arg table
        return (instr ++ [PRINT temp t])
translateClause(IFEClause cond scope1 scope2) table = do
        label1 <- newLabel
        label2 <- newLabel
        label3 <- newLabel
        instr1 <- translateCondition cond table label1 label2
        instr2 <- translateScope scope1 table
        instr3 <- translateScope scope2 table
        return (instr1 ++ [LABEL label1] ++ instr2 ++ [JUMP label3] ++ [LABEL label2] ++ instr3 ++ [LABEL label3])

translateScope :: Scope -> SymbolTable -> State Count [Instruction]
translateScope (Clauses []) _ = return []
translateScope (Clauses (x:xs)) table = do
    newTable <- genTable table (Clauses (x:xs))
    instr1 <- translateClause x newTable
    instr2 <- translateScope (Clauses xs) newTable
    return (instr1 ++ instr2)

translateAST :: Scope -> State Count [Instruction]
translateAST (Clauses []) = return []
translateAST scope = do
    table <- genTable setupTable scope
    translateScope scope table

genTable :: SymbolTable -> Scope -> State Count SymbolTable
genTable table (Clauses []) = return table 
genTable table (Clauses ((VarAttribution id Integer64Type _):xs)) = do
    temp <- newTemp
    let table = insertEntry id (TEMPVALUE temp IntegerType) table
    genTable table (Clauses xs)   
genTable table (Clauses ((VarAttribution id Float64Type _):xs)) = do 
    temp <- newTemp
    let table = insertEntry id (TEMPVALUE temp FloatType) table
    genTable table (Clauses xs)
genTable table (Clauses ((VarAttribution id t _):xs)) = do
    temp <- newTemp
    let table = insertEntry id (TEMPVALUE temp t) table
    genTable table (Clauses xs)
genTable table (Clauses ((ValAttribution id Integer64Type _):xs)) = do
    temp <- newTemp
    let table = insertEntry id (TEMPVALUE temp IntegerType) table
    genTable table (Clauses xs)
genTable table (Clauses ((ValAttribution id Float64Type _):xs)) = do
    temp <- newTemp
    let table = insertEntry id (TEMPVALUE temp FloatType) table
    genTable table (Clauses xs)
genTable table (Clauses ((ValAttribution id t _):xs)) = do
    temp <- newTemp
    let table = insertEntry id (TEMPVALUE temp t) table
    genTable table (Clauses xs)
genTable table (Clauses(_:xs)) = genTable table (Clauses xs)
