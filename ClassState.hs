module ClassState
where

import Data.Map (Map)
import qualified Data.Map as Map

-- Utilizat pentru a obține informații despre Variabile sau Funcții
data InstrType = Var | Func  deriving (Show, Eq)

-- TODO - Trebuie definit ClassState
data ClassState = ClassState (Map String [[String]]) deriving Show

initEmptyClass :: ClassState
initEmptyClass = ClassState (Map.insert "Var" [] (Map.insert "Func" [] Map.empty))

insertIntoClass :: ClassState -> InstrType -> [String] -> ClassState
insertIntoClass (ClassState cl) instr strl = ClassState (Map.insertWith (++) (show instr) [strl] cl)

getValues :: ClassState -> InstrType -> [[String]]
getValues (ClassState cl) instr = Map.findWithDefault [[]] (show instr) cl
