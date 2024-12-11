module SymbolTable where

import Data.Map (Map, toList)
import qualified Data.Map as Map
import Parser (Clause(..), PrimitiveType(..), Argument(..), Scope(..))


type Temp = String
type VALUE = Bool 
data STI = TEMPVALUE Temp | VARINFO PrimitiveType VALUE 
  deriving (Show, Eq)
type SymbolTable = Map String STI

setupTable :: SymbolTable
setupTable = Map.empty

createTable :: SymbolTable -> Scope ->  SymbolTable
createTable table (Clauses []) = table
createTable table (Clauses ((VarAttribution id t _):xs)) = createTable (insertEntry id (VARINFO t True) table) (Clauses xs)
createTable table (Clauses ((ValAttribution id t _):xs)) = createTable (insertEntry id (VARINFO t False) table) (Clauses xs)
createTable table (Clauses (_:xs)) = createTable table (Clauses xs)

insertEntry :: String -> STI -> SymbolTable -> SymbolTable
insertEntry id i table = Map.insert id i table

find :: String -> SymbolTable -> Maybe STI
find id table = Map.lookup id table



showTable :: SymbolTable -> FilePath -> IO ()
showTable table file = do
  appendFile file (unlines $ map show (toList table))

numPrimitiveType :: [PrimitiveType]
numPrimitiveType = [IntegerType, Integer64Type, FloatType, Float64Type]

handleNumPrimitiveType :: PrimitiveType -> PrimitiveType -> Maybe PrimitiveType
handleNumPrimitiveType t1 t2 
  | t1 == IntegerType && t2 == IntegerType = Just IntegerType
  | t1 == IntegerType && t2 == Integer64Type = Just Integer64Type
  | t1 == IntegerType && t2 == FloatType = Just FloatType
  | t1 == IntegerType && t2 == Float64Type = Just Float64Type
  | t1 == Integer64Type && t2 == IntegerType = Just Integer64Type
  | t1 == Integer64Type && t2 == Integer64Type = Just Integer64Type
  | t1 == Integer64Type && t2 == FloatType = Just Float64Type
  | t1 == Integer64Type && t2 == Float64Type = Just Float64Type
  | t1 == FloatType && t2 == IntegerType = Just FloatType
  | t1 == FloatType && t2 == Integer64Type = Just Float64Type
  | t1 == FloatType && t2 == FloatType = Just FloatType
  | t1 == FloatType && t2 == Float64Type = Just Float64Type
  | t1 == Float64Type && t2 == IntegerType = Just Float64Type
  | t1 == Float64Type && t2 == Integer64Type = Just Float64Type
  | t1 == Float64Type && t2 == FloatType = Just Float64Type
  | t1 == Float64Type && t2 == Float64Type = Just Float64Type
  | otherwise = Nothing

verifyArguments :: SymbolTable -> Argument -> PrimitiveType 
verifyArguments _ (Num _) = IntegerType
verifyArguments _ (Real _) = Float64Type
verifyArguments table (Id id) = case (find id table) of
  Just (VARINFO t _) -> t
  Just (TEMPVALUE _) -> error "Temp value in arguments"
  Nothing -> error "Variable not found in arguments"
verifyArguments _ (BooleanValue _) = BoolType
verifyArguments _ (Char _) = CharType
verifyArguments _ (Phrase _) = StringType
verifyArguments table (Sum a1 a2) =
  let t1 = verifyArguments table a1
      t2 = verifyArguments table a2
  in case handleNumPrimitiveType t1 t2 of
    Just t -> t
    Nothing -> error "Invalid type in addition"
verifyArguments table (Sub a1 a2) = 
  let t1 = verifyArguments table a1
      t2 = verifyArguments table a2
  in case handleNumPrimitiveType t1 t2 of
    Just t -> t
    Nothing -> error "Invalid type in subtraction"
verifyArguments table (Mult a1 a2) =
  let t1 = verifyArguments table a1
      t2 = verifyArguments table a2
  in case handleNumPrimitiveType t1 t2 of
    Just t -> t
    Nothing -> error "Invalid type in multiplication"
verifyArguments table (Div a1 a2) =
  let t1 = verifyArguments table a1
      t2 = verifyArguments table a2
  in case handleNumPrimitiveType t1 t2 of
    Just t -> t
    Nothing -> error "Invalid type in division"
verifyArguments table (Modulus a1 a2) =
  let t1 = verifyArguments table a1
      t2 = verifyArguments table a2
  in case handleNumPrimitiveType t1 t2 of
    Just t -> t
    Nothing -> error "Invalid type in modulus"
verifyArguments table (And a1 a2) = 
  let t1 = verifyArguments table a1
      t2 = verifyArguments table a2
  in if t1 == BoolType && t2 == BoolType then BoolType else error "Invalid type in and"
verifyArguments table (Or a1 a2) =
  let t1 = verifyArguments table a1
      t2 = verifyArguments table a2
  in if t1 == BoolType && t2 == BoolType then BoolType else error "Invalid type in or"
verifyArguments table (Not a) =
  let t = verifyArguments table a
  in if t == BoolType then BoolType else error "Invalid type in not"
verifyArguments table (Equals a1 a2) =
  let t1 = verifyArguments table a1
      t2 = verifyArguments table a2
  in if t1 == t2 then BoolType else error "Invalid type in equals"
verifyArguments table (NotEquals a1 a2) =
  let t1 = verifyArguments table a1
      t2 = verifyArguments table a2
  in if t1 == t2 then BoolType else error "Invalid type in not equals"
verifyArguments table (Lesser a1 a2) =
  let t1 = verifyArguments table a1
      t2 = verifyArguments table a2
  in if (t1 `elem` numPrimitiveType && t2 `elem` numPrimitiveType) || (t1 == CharType && t2 == CharType) || (t1 == StringType && t2 == StringType) then BoolType else error "Invalid type in lesser"
verifyArguments table (LesserEquals a1 a2) =
  let t1 = verifyArguments table a1
      t2 = verifyArguments table a2
  in if (t1 `elem` numPrimitiveType && t2 `elem` numPrimitiveType) || (t1 == CharType && t2 == CharType) || (t1 == StringType && t2 == StringType) then BoolType else error "Invalid type in lesser equals"
verifyArguments table (Greater a1 a2) =
  let t1 = verifyArguments table a1
      t2 = verifyArguments table a2
  in if (t1 `elem` numPrimitiveType && t2 `elem` numPrimitiveType) || (t1 == CharType && t2 == CharType) || (t1 == StringType && t2 == StringType) then BoolType else error "Invalid type in greater"
verifyArguments table (GreaterEquals a1 a2) =
  let t1 = verifyArguments table a1
      t2 = verifyArguments table a2
  in if (t1 `elem` numPrimitiveType && t2 `elem` numPrimitiveType) || (t1 == CharType && t2 == CharType) || (t1 == StringType && t2 == StringType) then BoolType else error "Invalid type in greater equals"

verifyClause :: SymbolTable -> Clause -> Bool
verifyClause table (VarAttribution id _ a) = 
  case find id table of
    Just (VARINFO t _) -> if verifyArguments table a == t then True else error "Invalid type in attribution"
    Just (TEMPVALUE _) -> error "Temp value in attribution"
    Nothing -> error "Variable not found in attribution"
verifyClause table (ValAttribution id _ a) =
  case find id table of
    Just (VARINFO t _) -> if verifyArguments table a == t then True else error "Invalid type in attribution"
    Just (TEMPVALUE _) -> error "Temp value in attribution"
    Nothing -> error "Variable not found in attribution"
verifyClause table (IFClause a scope) = 
  let t = verifyArguments table a
      verify = verifyScope (createTable table scope) scope
  in if t == BoolType && verify then True else error "Invalid type in if clause"
verifyClause table (IFEClause a scope1 scope2) = 
  let t = verifyArguments table a
      verify1 = verifyScope (createTable table scope1) scope1
      verify2 = verifyScope (createTable table scope2) scope2
  in if t == BoolType && verify1 && verify2 then True else error "Invalid type in if else clause"
verifyClause table (WhileClause a scope) =
  let t = verifyArguments table a
      verify = verifyScope (createTable table scope) scope
  in if t == BoolType && verify then True else error "Invalid type in while clause"
verifyClause table (Assign id a) =
  case find id table of
    Just (VARINFO t canAssign) -> if not canAssign then error "Cannot assign to a constant" else if verifyArguments table a == t then True else error "Invalid type in assignment"
    Just (TEMPVALUE _) -> error "Temp value in assignment"
    Nothing -> error "Variable not found in assignment"
verifyClause _ (PrintClause _) = True

verifyScope :: SymbolTable -> Scope -> Bool
verifyScope _ (Clauses []) = True
verifyScope table (Clauses (x:xs)) = (verifyClause table x) && (verifyScope table (Clauses xs))

verifyAST :: Scope -> Bool
verifyAST (Clauses []) = True
verifyAST scope = verifyScope (createTable setupTable scope) scope
