module Parser
where

import Util
import Data.Maybe
import InferenceDataType

import Data.Map (Map)
import qualified Data.Map as Map

-- Definire Program
data Program = Program (Map String [[String]]) deriving Show

initEmptyProgram :: Program
initEmptyProgram = Program (Map.insert "Var" [] (Map.insert "Cls" [["Global", "Global"]] Map.empty))

getVars :: Program -> [[String]]
getVars (Program prog) = Map.findWithDefault [[]] "Var" prog

getEverySnd :: [[String]] -> [String]
getEverySnd strlst = [x2 | x1:x2:xs <- strlst]

getClasses :: Program -> [String]
getClasses (Program prog) = getEverySnd (Map.findWithDefault [[]] "Cls" prog)

getParentClass :: String -> Program -> String
getParentClass clsname (Program prog) = head [x1 | x1:x2:xs <- (Map.findWithDefault [[]] "Cls" prog), x2 == clsname]

getFuncsForClass :: String -> Program -> [[String]]
getFuncsForClass = undefined

-- Instruction poate fi ce considerați voi
data Instruction = Instruction
parse :: String -> [Instruction]
parse = undefined

interpret :: Instruction -> Program -> Program
interpret = undefined

infer :: Expr -> Program -> Maybe String
infer = undefined


