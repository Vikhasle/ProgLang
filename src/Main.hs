module Main where

import Parser
import System.Environment
import System.IO

import Control.Monad.State
import qualified Data.Map.Strict as M
import Data.Map.Strict (Map)

type VarMem = Map String PValue
type Interpreter a = State Varmem a


eval :: PValue VarMem -> PValue
eval (PString val) _ = PString val
eval (PNumber val) _ = PNumber val
eval (PBool val) _ = PBool val
eval (PVar var) bindings = case M.lookup var bindings of 
							Just v -> v
							Nothing -> PError "Variable " ++ var ++ " not in scope!"
eval (PList xs) _ = PList $ map eval xs
eval (PExpr (PVar func : args)) bindings = apply func $ map (map eval args) bindings
eval (PExpr []) = PNumber 0

apply :: String -> [PValue] -> PValue
apply func args = maybe (PBool False) ($ args) $ lookup func primitives

primitives :: [(String, [PValue] -> PValue)]
primitives = [("add", numericBinop (+)),
			  ("sub", numericBinop (-)),
			  ("mul", numericBinop (*)),
			  ("div", numericBinop div),
			  ("mod", numericBinop mod),
			  ("len", len),
			  ("equ", equ) 
			  ("let", assign)
			  ]

len :: [PValue] -> PValue
len xs = PNumber $ length xs

equ :: [PValue] -> PValue
equ (lhs : rhs : _) = PBool $ (unpackBool $ eval lhs) == (unpackBool $ eval rhs)

assign :: String -> [PValue] -> PValue
assign (var_name : var_exp : _ ) = eval var_exp

numericBinop :: (Int -> Int -> Int) -> [PValue] -> PValue
numericBinop op params = PNumber $ foldl1 op $ map unpackNum params

unpackNum :: PValue -> Int
unpackNum (PNumber n) = n
unpackNum _ = 0 -- TODO add error here

unpackVar :: PValue -> String
unpackVar (PVar x) = x

unpackBool :: PValue -> Bool
unpackBool (PBool x) = x

flushStr :: String -> IO()
flushStr str = putStr str >> hFlush stdout

readLine :: String -> IO String
readLine promptHead = flushStr promptHead >> getLine

evalString :: Intrepeter -> String -> PrgMem -> IO String
evalString expr = return $ show $ fmap eval $ parsePrg expr

evalAndPrint :: String -> IO ()
evalAndPrint expr =  evalString expr >>= putStrLn

repeat_ cond prompt action mem = do
	result <- prompt
	if cond result 
		then return ()
		else action result mem >> repeat_ cond prompt action mem

repl :: IO ()
repl = repeat_ (== "exit") (readLine "|> ") evalAndPrint PrgMem
	
	
runFile :: String -> IO()
runFile filename = do
		prg <- readFile filename
		print $ map eval $ parsePrg prg

main :: IO ()
main = do
	args <- getArgs 
	case length args of 
		0 -> repl
--		1 -> runFile $ fst args
