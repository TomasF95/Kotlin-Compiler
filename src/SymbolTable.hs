module SymbolTable (
    Symbol(..),
    SymbolTable,
    empty,
    insertSymbol,
    lookupSymbol,
    symbolExists
) where

import qualified Data.Map as Map
import Data.Maybe (fromMaybe)

data Symbol = Symbol {
    name :: String,     -- Name of the symbol
    symType :: String,  -- Type of the symbol
    mutable :: Bool     -- Whether the symbol is mutable    
} deriving (Show)

type SymbolTable = Map.Map String Symbol

empty :: SymbolTable
empty = Map.empty

insertSymbol :: SymbolTable -> Symbol -> SymbolTable
insertSymbol table symbol = Map.insert (name symbol) symbol table

lookupSymbol :: SymbolTable -> String -> Maybe Symbol
lookupSymbol table symbolName = Map.lookup symbolName table

symbolExists :: SymbolTable -> String -> Bool
symbolExists table symbolName = Map.member symbolName table
