import System.Environment
import Data.List
import Debug.Trace

data Token
	= Stop
	| StartLst
	| EndLst
	| Num Int
	| Str String
deriving (Show)


type AssocList = [(String, Token)]

data Stmt = Operation Token deriving (Show)
tokenizer :: [String] -> [Token]
tokenizer program_string = map token? $ map split

token? :: String -> [Token] -> [Token]
token? "" tokens = tokens
token? ('[':xs) tokens = token? xs (StartLst : tokens)
token? ('.':xs) tokens = token? xs (Stop : tokens)
token? 

parser :: [Token] -> [Stmt]
evaluator :: [Stmt] -> IO()

evaluate :: Stmt -> Var

eval_add [] = 0
eval_add (x:(y:xs)) = [x+y] ++ xs



