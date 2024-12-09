module SymbolTable where

import           Data.Map (Map, toList)
import qualified Data.Map as Map
import          Parser   (Clause (..), Argument(..), PrimitiveType (..), Scope (..))
import Control.Exception (handle)

type SymbolTable = Map String PrimitiveType

verifyAST :: Scope -> Bool
verifyAST (Clauses []) = True
verifyAST scope = verifyScope (setupTable createTable scope) scope

setupTable :: SymbolTable -> Scope -> SymbolTable
setupTable table (Clauses []) = table
setupTable table (Clauses ((VarAttribution id typ _):xs)) = setupTable (insertEntry id typ table) (Clauses xs)
setupTable table (Clauses ((ValAttribution id typ _):xs)) = setupTable (insertEntry id typ table) (Clauses xs)
setupTable table (Clauses (_:xs)) = setupTable table (Clauses xs)

createTable :: SymbolTable
createTable = Map.empty

verifyScope :: SymbolTable -> Scope -> Bool
verifyScope _ (Clauses []) = True
verifyScope x (Clauses (cls:xs)) = verifyArgument x cls && verifyScope x (Clauses xs)

insertEntry :: String -> PrimitiveType -> SymbolTable -> SymbolTable
insertEntry id typ table = Map.insert id typ table

find :: String -> SymbolTable -> Maybe PrimitiveType
find id table = Map.lookup id table

showTable :: SymbolTable -> FilePath -> IO ()
showTable table file = do
  appendFile file (unlines $ map show (toList table))

numPrimitiveType :: [PrimitiveType]
numPrimitiveType = [IntegerType, Integer64Type, FloatType, Float64Type]

handlePrimitiveTypeConflict :: PrimitiveType -> PrimitiveType -> Maybe PrimitiveType
handlePrimitiveTypeConflict x y 
  | x == IntegerType && y == IntegerType = Just IntegerType
  | x == IntegerType && y == Integer64Type = Just Integer64Type
  | x == IntegerType && y == FloatType = Just FloatType
  | x == IntegerType && y == Float64Type = Just Float64Type
  | x == Integer64Type && y == IntegerType = Just Integer64Type
  | x == Integer64Type && y == Integer64Type = Just Integer64Type
  | x == Integer64Type && y == FloatType = Just FloatType
  | x == Integer64Type && y == Float64Type = Just Float64Type
  | x == FloatType && y == IntegerType = Just FloatType
  | x == FloatType && y == Integer64Type = Just FloatType
  | x == FloatType && y == FloatType = Just FloatType
  | x == FloatType && y == Float64Type = Just Float64Type
  | x == Float64Type && y == IntegerType = Just Float64Type
  | x == Float64Type && y == Integer64Type = Just Float64Type
  | x == Float64Type && y == FloatType = Just Float64Type
  | x == Float64Type && y == Float64Type = Just Float64Type
  | otherwise = Nothing


verifyArgument :: SymbolTable -> Clause -> Bool
verifyArgument x (VarAttribution id typ arg)
  = case find id x of
      Just typ -> if verifyClause x arg == typ then True else error "Type mismatch in VarAttribution"
      Nothing -> error "Variable not declared"
verifyArgument x (ValAttribution id typ arg)
  = case find id x of
      Just typ -> if verifyClause x arg == typ then True else error "Type mismatch in ValDeclaration"
      Nothing -> error "Variable not declared"
verifyArgument x (Assign id arg) 
  = case find id x of
      Just typ -> if verifyClause x arg == typ then True else error "Type mismatch in Assign"
      Nothing -> error "Variable not declared"
verifyArgument x (IFClause arg scope)
  = let condition = verifyClause x arg
        check = verifyScope (setupTable x scope) scope
    in if condition == BoolType then check else error "Type mismatch in IfClause"
verifyArgument x (WhileClause arg scope)
  = let condition = verifyClause x arg
        check = verifyScope x scope
    in if condition == BoolType then check else error "Type mismatch in WhileClause"
verifyArgument x (PrintClause arg) = True
verifyArgument x (IFEClause arg scope1 scope2)
  = let condition = verifyClause x arg
        check1 = verifyScope (setupTable x scope1) scope1
        check2 = verifyScope (setupTable x scope2) scope2
    in if condition == BoolType then check1 && check2 else error "Type mismatch in IFEClause"


    

verifyClause :: SymbolTable -> Argument -> PrimitiveType
verifyClause _ (Num _) = IntegerType
verifyClause _ (Real _) = Float64Type
verifyClause x (Id id) = case find id x of
  Just typ -> typ
  Nothing -> error "Variable not declared"
verifyClause _ (BooleanValue _) = BoolType
verifyClause _ (Char _) = CharType
verifyClause _ (Phrase _) = StringType
verifyClause x (Sum arg1 arg2) = 
  let typ1 = verifyClause x arg1
      typ2 = verifyClause x arg2
  in case handlePrimitiveTypeConflict typ1 typ2 of
    Just typ -> typ
    Nothing -> error "Type mismatch in Sum"
verifyClause x (Sub arg1 arg2) =
  let typ1 = verifyClause x arg1
      typ2 = verifyClause x arg2
  in case handlePrimitiveTypeConflict typ1 typ2 of
    Just typ -> typ
    Nothing -> error "Type mismatch in Sub"
verifyClause x (Mult arg1 arg2) =
  let typ1 = verifyClause x arg1
      typ2 = verifyClause x arg2
  in case handlePrimitiveTypeConflict typ1 typ2 of
    Just typ -> typ
    Nothing -> error "Type mismatch in Mul"
verifyClause x (Div arg1 arg2) =
  let typ1 = verifyClause x arg1
      typ2 = verifyClause x arg2
  in case handlePrimitiveTypeConflict typ1 typ2 of
    Just typ -> typ
    Nothing -> error "Type mismatch in Div"
verifyClause x (Modulus arg1 arg2) =
  let typ1 = verifyClause x arg1
      typ2 = verifyClause x arg2
  in case handlePrimitiveTypeConflict typ1 typ2 of
    Just typ -> typ
    Nothing -> error "Type mismatch in Modulus"
verifyClause x (And arg1 arg2) =
  let typ1 = verifyClause x arg1
      typ2 = verifyClause x arg2
  in if typ1 == BoolType && typ2 == BoolType then BoolType else error "Type mismatch in And"
verifyClause x (Or arg1 arg2) =
  let typ1 = verifyClause x arg1
      typ2 = verifyClause x arg2
  in if typ1 == BoolType && typ2 == BoolType then BoolType else error "Type mismatch in Or"
verifyClause x (Not arg) =
  let typ = verifyClause x arg
  in if typ == BoolType then BoolType else error "Type mismatch in Not"
verifyClause x (Equals arg1 arg2) =
  let typ1 = verifyClause x arg1
      typ2 = verifyClause x arg2
  in if typ1 == typ2 then BoolType else error "Type mismatch in Equals"
verifyClause x (NotEquals arg1 arg2) =
  let typ1 = verifyClause x arg1
      typ2 = verifyClause x arg2
  in if typ1 == typ2 then BoolType else error "Type mismatch in NotEquals"
verifyClause x (Greater arg1 arg2) =
  let typ1 = verifyClause x arg1
      typ2 = verifyClause x arg2
  in case handlePrimitiveTypeConflict typ1 typ2 of
    Just typ -> BoolType
    Nothing -> error "Type mismatch in Greater"
verifyClause x (Lesser arg1 arg2) =
  let typ1 = verifyClause x arg1
      typ2 = verifyClause x arg2
  in if (typ1 `elem` numPrimitiveType && typ2 `elem` numPrimitiveType) || (typ1 == CharType && typ2 == CharType) || (typ1 == StringType && typ2 == StringType)
       then BoolType else error "Type mismatch in Lesser"
verifyClause x (Greater arg1 arg2) =
  let typ1 = verifyClause x arg1
      typ2 = verifyClause x arg2
  in if (typ1 `elem` numPrimitiveType && typ2 `elem` numPrimitiveType) || (typ1 == CharType && typ2 == CharType) || (typ1 == StringType && typ2 == StringType) 
        then BoolType else error "Type mismatch in Greater"
verifyClause x (LesserEquals arg1 arg2) =
  let typ1 = verifyClause x arg1
      typ2 = verifyClause x arg2
  in if (typ1 `elem` numPrimitiveType && typ2 `elem` numPrimitiveType) || (typ1 == CharType && typ2 == CharType) || (typ1 == StringType && typ2 == StringType)
        then BoolType else error "Type mismatch in LesserEquals"
verifyClause x (GreaterEquals arg1 arg2) =
  let typ1 = verifyClause x arg1
      typ2 = verifyClause x arg2
  in if (typ1 `elem` numPrimitiveType && typ2 `elem` numPrimitiveType) || (typ1 == CharType && typ2 == CharType) || (typ1 == StringType && typ2 == StringType)
        then BoolType else error "Type mismatch in GreaterEquals"
verifyClause x (Not arg) =
  let typ = verifyClause x arg
  in if typ == BoolType then BoolType else error "Type mismatch in Not"
verifyClause x ReadClause = StringType