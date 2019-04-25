-------------------------------------------------
-- Diaconescu Florin, 322CB, florin.diaconescu --
-------------------------------------------------

module Parser
where

import Util
import Data.Maybe
import InferenceDataType

import Data.Map (Map)
import qualified Data.Map as Map

import Data.Char
import Split
import Data.List

-- Definire Program
data Program = Program (Map String [[String]]) deriving Show

initEmptyProgram :: Program
initEmptyProgram = Program (Map.insert "Var" [] (Map.insert "Cls" [["Global", "Global"]] (Map.insert "Func" [] Map.empty)))

getVars :: Program -> [[String]]
getVars (Program prog) = Map.findWithDefault [] "Var" prog

getClasses :: Program -> [String]
getClasses (Program prog) = map (!!1) listClasses
    where listClasses = Map.findWithDefault [] "Cls" prog

getParentClass :: String -> Program -> String
getParentClass clsname (Program prog) = head [x1 | x1:x2:xs <- listClasses, x2 == clsname]
    where listClasses = Map.findWithDefault [] "Cls" prog

getFuncsForClass :: String -> Program -> [[String]]
getFuncsForClass clsname (Program prog) = [xs | x1:xs <- listClasses, x1 == clsname]
    where listClasses = Map.findWithDefault [] "Func" prog

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

getTypeOfVar :: String -> Program -> String
getTypeOfVar var (Program pr) = head [var_type | [var_name, var_type] <- getVars(Program pr), var_name == var]

recursiveCheck :: [Expr] -> Program -> Maybe String
recursiveCheck [] _ = Nothing
recursiveCheck (expr:[]) (Program pr) = infer expr (Program pr)
recursiveCheck (expr:rest) (Program pr)
    | ((infer expr (Program pr)) /= Nothing) = recursiveCheck rest (Program pr) 
    | otherwise = Nothing

infer :: Expr -> Program -> Maybe String
infer (Va var) (Program pr)
    | (var `elem` vars) = Just (getTypeOfVar var (Program pr)) 
    | otherwise = Nothing
    where vars = map head (getVars (Program pr))

infer (FCall var_sym func_sym nest_expr) (Program pr)
    | (func_sym `elem` funcs) = if ((recursiveCheck nest_expr (Program pr)) /= Nothing)
        then Just return_type else Nothing
    | (func_sym `elem` parent_funcs) = if ((recursiveCheck nest_expr (Program pr)) /= Nothing)
        then Just return_type_parent else Nothing
    | otherwise = Nothing
    where funcs = map head (getFuncsForClass var_type (Program pr))
          var_type = getTypeOfVar var_sym (Program pr)
          func_types = map (!!1) (getFuncsForClass var_type (Program pr))
          func_index = elemIndex func_sym funcs
          return_type = func_types!!(fromMaybe 0 func_index)
          parent_funcs = map head (getFuncsForClass (getParentClass var_type (Program pr))(Program pr))
          parent_types = map (!!1) (getFuncsForClass (getParentClass var_type (Program pr))(Program pr))
          parent_index = elemIndex func_sym parent_types
          return_type_parent = parent_types!!(fromMaybe 0 parent_index)