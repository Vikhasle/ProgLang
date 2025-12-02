module Parser where

import Text.Parsec



data PValue 
	= PString String
	| PNumber Int
	| PBool Bool
	| PVar String
	| PError String
	| PList [PValue]
	| PExpr [PValue]

showVal :: PValue -> String
showVal (PString contents) = "\"" ++ contents ++ "\""
showVal (PVar name) = name
showVal (PNumber contents) = show contents
showVal (PBool True) = "true"
showVal (PBool False) = "false"
showVal (PExpr vals) = unwords $ map showVal vals

instance Show PValue where show = showVal



valueParser :: Parsec String st PValue
valueParser = stringParser <|>
	  		  numberParser <|>
			  listParser   <|>
	  		  varParser   

stringParser :: Parsec String st PValue
stringParser = do 
	x <- between (char '"') (char '"') (many (noneOf "\""))
	return $ PString x

listParser :: Parsec String st PValue
listParser = do
	x <- between (char '[') (char ']') (valueParser `sepBy` (char ','))
	return $ PList x

varParser :: Parsec String st PValue
varParser = do
	x <- many1 letter
	return $ case x of
				"true" -> PBool True
				"false" -> PBool False
				_ -> PVar x


numberParser :: Parsec String st PValue
numberParser = do
	x <- read <$> (many1 $ oneOf "0123456789")
	return $ PNumber x

exprParser :: Parsec String st PValue
exprParser = do
	x <- valueParser `sepBy` (char ' ')
	return $ PExpr x

prgParser :: Parsec String st [PValue]
prgParser = exprParser `sepEndBy` (char '\n')

parseExpr :: String -> PValue
parseExpr input = case parse exprParser "" input of
	Left err ->  PString $ "No expr match: " ++ show err
	Right val -> val

parsePrg :: String -> [PValue]
parsePrg input = case parse prgParser "" input of
	Left err -> [PString $ "No expr match: " ++ show err]
	Right val -> val

