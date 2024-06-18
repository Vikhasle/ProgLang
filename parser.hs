import Text.ParserCombinators.Parsec
import Numeric 

data PValue 
	= PString String
	| PNumber Double
	| PBool Bool
	| PList (PAry PValue)
	deriving (Eq, Ord, Show)

newtype PAry a = PAry {
	fromPAry :: [a]
} deriving (Eq, Ord, Show)

word :: Parser String
word = many1 letter

p_text :: CharParser () PValue
p_text = spaces *> text
     <?> "Program text"
    where text = PValue <$> p_value
				<|> PList <$> p_list

p_value :: CharParser () PValue
p_value = value <* spaces
  where value = PString <$> p_string
            <|> PNumber <$> p_number
            <|> PList <$> p_list
            <|> PBool <$> p_bool
            <?> "Value"
p_value_choice = value <* spaces
  where value = choice [ PString <$> p_string
                       , PNumber <$> p_number
            	       , PList <$> p_list
                       , PBool <$> p_bool
                       ]
       				   <?> "Value"

p_list :: CharParser () (PAry PValue)
p_list = PAry <$> p_series '[' p_value ']'

p_series :: Char -> CharParser () a -> Char -> CharParser () [a]
p_series left parser right =
	between (char left <* spaces) (char right) $
			(parser <* spaces) `sepBy` (char ',' <* spaces)

p_bool :: CharParser () Bool
p_bool = True <$ string "true"
     <|> False <$ string "false"

p_number :: CharParser () Double
p_number = do s <- getInput
              case readSigned readFloat s of
                [(n, s')] -> n <$ setInput s'
                _         -> pzero

p_string :: CharParser () String
p_string = between (char '\"') (char '\"') (many pchar)
    where pchar = char '\\' *> (p_escape <|> p_unicode)
              <|> satisfy (`notElem` "\"\\")

p_escape :: CharParser () Char
p_escape = choice (zipWith decode "bnfrt\\\"/" "\b\n\f\r\t\\\"/")

decode :: Char -> Char -> CharParser () Char
decode c r = r <$ char c

p_unicode :: CharParser () Char
p_unicode = char 'u' *> (decode <$> count 4 hexDigit)
    where decode x = toEnum code
              where ((code,_):_) = readHex x

eol :: GenParser Char st Char
eol = char '\n'

{--parsePrg :: String -> Either ParseError [[String]]
parsePrg input = parse prgFile "(TODO Add errors)" input

parseFile filename = 
	do fstring <- readFile filename 
	   return (parsePrg fstring)
--}
