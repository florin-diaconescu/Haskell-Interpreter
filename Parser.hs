module Parser
where

import Util
import Data.Maybe
import InferenceDataType

import Data.Map (Map)
import qualified Data.Map as Map

import Data.Char
import Split

-- Definire Program
data Program = Program (Map String [[String]]) deriving Show

initEmptyProgram :: Program
initEmptyProgram = Program (Map.insert "Var" [] (Map.insert "Cls" [["Global", "Global"]] (Map.insert "Func" [] Map.empty)))

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
getFuncsForClass clsname (Program prog) = [xs | x1:xs <- listClasses, x1 == clsname]
    where listClasses = Map.findWithDefault [[]] "Func" prog

-- Instruction poate fi ce considerați voi
type Instruction = String

parse :: String -> [Instruction]
parse input = lines input

lowerString :: String -> String
lowerString str = [(toLower chr) | chr <- str]

checkParameters :: [String] -> Program -> Bool
checkParameters [] (Program pr) = True
checkParameters strLst (Program pr) = ((head strLst) `elem` classes) && (checkParameters (tail strLst) (Program pr))
    where classes = getClasses (Program pr)

interpret :: Instruction -> Program -> Program
interpret instr (Program pr)
    | ((length (words instr)) == 0) = 
        Program pr

    | (lowerString (head (words instr)) == "class") &&
        ((length (words instr)) == 4) &&
        ((words instr)!!3 `elem` classes) &&
        ((words instr)!!1 `notElem` classes) =
        Program (Map.insertWith (++) "Cls" [(words instr)!!3 : [(words instr)!!1]] pr)

    | (lowerString (head (words instr)) == "class") &&
        ((words instr)!!1 `notElem` classes) =
        Program (Map.insertWith (++) "Cls" ["Global": [(words instr)!!1]] pr)

    | (lowerString (head (splitOneOf " =" instr)) == "newvar") &&
       (last (splitOneOf " =" instr) `elem` classes) =
       Program (Map.insertWith (++) "Var" [(splitOneOf " =" instr)!!1 : [last (splitOneOf " =" instr)]] pr)

    | (head fltStr `elem` classes) &&
        (checkParameters (drop 3 fltStr) (Program pr)) &&
        (fltStr!!1 `elem` classes) =
        Program (Map.insertWith (++) "Func" [(fltStr!!1) : (fltStr!!2) : (head fltStr) : (drop 3 fltStr)] pr)
       
    | otherwise = Program pr
        where fltStr = filter (not.null) (splitOneOf " :()," instr)
              classes = getClasses (Program pr)

infer :: Expr -> Program -> Maybe String
infer _ _ = Nothing
