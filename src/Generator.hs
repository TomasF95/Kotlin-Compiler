import Control.Monad.Statement
import Parser(Clause(..), Argument(..), PrimitiveType(..), Scope(..))

data Instruction = COPY Register Register
                 | COPYI Register Integer
                 | COPYR Register Double
                 | COPYB Register Bool
                 | COPYS Register String
                 | BINARY BinOperator Register Register Register
                 | MARK LabelName
                 | GOTO LabelName
                 | IFCOND Register BinOperator Register LabelName LabelName
                 | UNARY UnOperator Register Register
                 | IFUNARY Register LabelName LabelName
                 | DISPLAY Register
                 deriving Show

data BinOperator = Sum | Sub | Mult | Div | Modulus | Lesser | LesserEquals | Equals | NotEquals | And| Or
                 deriving Show

data UnOperator = Not
                deriving Show

type Register = String
type LabelName = String

type Count = (Int, Int)

newRegister :: State Count Register
newRegister = do
    (registers, labels) <- get
    put (registers + 1, labels)
    return ("r" ++ show registers)

newLabel :: State Count LabelName
newLabel = do
    (registers, labels) <- get
    put (registers, labels + 1)
    return ("L" ++ show labels)

translateArgument :: Argument -> SymbolTable -> Register -> State Count [Instruction]
translateArgument (Num x) _ xs = return [COPYI xs x]
translateArgument (Real x) _ xs = return [COPYR xs x]
translateArgument (BooleanValue x) _ xs = return [COPYB xs x]
translateArgument (Char x) _ xs = return [COPYS xs x]
translateArgument (Phrase x) _ xs = return [COPYS xs x]
translateArgument (Id x) context xs = do
    let Just t = find x context
    return [COPY xs t]
translateArgument (Sum x y) context xs = translateBinOperator Sum x y context xs
translateArgument (Sub x y) context xs = translateBinOperator Sub x y context xs
translateArgument (Mult x y) context xs = translateBinOperator Mult x y context xs
translateArgument (Div x y) context xs = translateBinOperator Div x y context xs
translateArgument (Modulus x y) context xs = translateBinOperator Modulus x y context xs
translateArgument (And x y) context xs = translateBinOperator And x y context xs
translateArgument (Or x y) context xs = translateBinOperator Or x y context xs
translateArgument (Not x) context xs = do
    typ1 -> newRegister
    instr1 <- translateArgument x context typ1
    return instr1 ++ [BINARY op xs typ1]
translateArgument (Equals x y) context xs = translateBinOperator Equals x y context xs
translateArgument (NotEquals x y) context xs = translateBinOperator NotEquals x y context xs
translateArgument (Lesser x y) context xs = translateBinOperator Lesser x y context xs
translateArgument (LesserEquals x y) context xs = translateBinOperator LesserEquals x y context xs
translateArgument (Greater x y) context xs = translateBinOperator Greater x y context xs
translateArgument (GreaterEquals x y) context xs = translateBinOperator GreaterEquals x y context xs

translateBinOperator :: BinOperator -> Argument -> Argument -> SymbolTable -> Register -> State Count [Instruction]
translateBinOperator op x y context xs = do
    typ1 <- newRegister
    typ2 <- newRegister
    instr1 <- translateArgument x context typ1
    instr2 <- translateArgument y context typ2
    return (instr1 ++ instr2 ++ [BINARY op xs typ1 typ2])



translateBinaryCondition :: BinOperator -> Argument -> Argument -> SymbolTable -> LabelName -> LabelName -> State Count [Instruction]
translateBinaryCondition op x y context trueLabel falseLabel = do
    typ1 <- newRegister
    typ2 <- newRegister
    instr1 <- translateArgument x context typ1
    instr2 <- translateArgument y context typ2
    return (instr1 ++ instr2 ++ [IFCOND typ1 op typ2 trueLabel falseLabel])

translateCondition :: Argument -> SymbolTable -> LabelName -> LabelName -> State Count [Instruction]
translateCondition (And x y) context trueLabel falseLabel = translateBinaryCondition And x y context trueLabel falseLabel
translateCondition (Or x y) context trueLabel falseLabel = translateBinaryCondition Or x y context trueLabel falseLabel
translateCondition (Not x) context trueLabel falseLabel = do
    typ1 <- newRegister
    instruction <- translateArgument x context typ1
    return (instruction ++ [IFUNARY typ1 trueLabel falseLabel])
translateCondition (Equals x y) context trueLabel falseLabel = translateBinaryCondition Equals x y context trueLabel falseLabel
translateCondition (Lesser x y) context trueLabel falseLabel = translateBinaryCondition Lesser x y context trueLabel falseLabel
translateCondition (LesserEquals x y) context trueLabel falseLabel = translateBinaryCondition LesserEquals x y context trueLabel falseLabel
translateCondition (Greater x y) context trueLabel falseLabel = translateBinaryCondition Greater x y context trueLabel falseLabel
translateCondition (GreaterEquals x y) context trueLabel falseLabel = translateBinaryCondition GreaterEquals x y context trueLabel falseLabel
translateCondition (NotEquals x y) context trueLabel falseLabel = translateBinaryCondition NotEquals x y context trueLabel falseLabel
translateCondition ( Id id) context trueLabel falseLabel = do
    let Just t = find id context
    return [IFUNARY t trueLabel falseLabel]

translateScope :: [Clause] -> SymbolTable -> State Count [Instruction]
translateScope [] _ = return []
translateScope (x:xs) context = do
    instr1 <- translateClause x context
    instr2 <- translateScope xs context
    return (instr1 ++ instr2)

translateClause :: Clause -> SymbolTable -> State Count [Instruction]
translateClause (ValAttribution id _ arg) context = do
    let Just t = find id context
    translateArgument arg context t
translateClause (VarAttribution id _ arg) context = do
    let Just t = find id context
    translateArgument arg context t
translateClause (WhileClause condition scope) context = do
    startLabel <- newLabel
    falseLabel <- newLabel
    finalLabel <- newLabel
    instr1 <- translateCondition condition context trueLabel finalLabel
    instr2 <- translateScope scope context
    return ([MARK trueLabel] ++ instr2 ++ instr1 ++ [GOTO trueLabel] ++ [MARK falseLabel])
translateClause (IFClause condition scope) context = do
    trueLabel <- newLabel
    falseLabel <- newLabel
    instr1 <- translateCondition condition context trueLabel falseLabel
    instr2 <- translateScope scope context
    return (instr1 ++ [MARK trueLabel] ++ instr2 ++ [MARK falseLabel])
translateClause (IFEClause condition scope1 scope2) context = do
    trueLabel <- newLabel
    falseLabel <- newLabel
    finallabel <- newLabel
    instr1 <- translateCondition condition context trueLabel falseLabel
    instr2 <- translateScope scope1 context
    instr3 <- translateScope scope2 context
    return (instr1 ++ [MARK trueLabel] ++ instr2 ++ [GOTO finalLabel] ++ [MARK falseLabel] ++ instr3 ++ [MARK finalLabel])
translateClause (PrintClause arg) context = do
    typ1 <- newRegister
    instr1 <- translateArgument arg context typ1
    return (instr1 ++ [DISPLAY typ1])
translateClause (Assign id arg) context = do
    let Just t = find id context
    translateArgument arg context t