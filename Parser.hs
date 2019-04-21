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
getClasses (Program prog) = getEverySnd listClasses
    where listClasses = Map.findWithDefault [[]] "Cls" prog

getParentClass :: String -> Program -> String
getParentClass clsname (Program prog) = head [x1 | x1:x2:xs <- listClasses, x2 == clsname]
    where listClasses = Map.findWithDefault [[]] "Cls" prog

getFuncsForClass :: String -> Program -> [[String]]
getFuncsForClass clsname (Program prog)
    | funcList == [[]] = []
    | otherwise = funcList
    where listClasses = Map.findWithDefault [[]] "Cls" prog
          funcList = [xs | x1:x2:xs <- listClasses, x2 ==  clsname]

-- Instruction poate fi ce considerați voi
data Instruction = Instruction
parse :: String -> [Instruction]
parse = undefined

interpret :: Instruction -> Program -> Program
interpret = undefined

infer :: Expr -> Program -> Maybe String
infer = undefined


