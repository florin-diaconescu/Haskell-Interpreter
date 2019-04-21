module Parser
where

import Util
import Data.Maybe
import InferenceDataType

import Data.Map (Map)
import qualified Data.Map as Map

import Data.Char

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
type Instruction = String

parse :: String -> [Instruction]
parse input = lines input

lowerString :: String -> String
lowerString str = [(toLower chr) | chr <- str]

interpret :: Instruction -> Program -> Program
interpret instr (Program pr)
    | (length (words instr)) == 0 = Program pr
    | (lowerString (head (words instr)) == "class") && ((length (words instr)) == 4) =
        Program (Map.insertWith (++) "Cls" [(words instr)!!3 : [(words instr)!!1]] pr)

    | (lowerString (head (words instr)) == "class") &&  ((length (words instr)) == 2) =
        Program (Map.insertWith (++) "Cls" ["Global": [words(instr)!!1]] pr)

    | (lowerString (head (words instr)) == "newvar") && ((length (words instr)) == 4) &&
       ((words instr)!!3 `elem` (getClasses (Program pr))) =
       Program (Map.insertWith (++) "Var" [(words instr)!!1 : [(words instr)!!3]] pr)

    | otherwise = Program pr

infer :: Expr -> Program -> Maybe String
infer _ _ = Nothing
