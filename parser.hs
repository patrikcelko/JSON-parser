import Text.Read (readMaybe)
import Text.Parsec
import Text.Parsec.String
import System.Environment
import Data.Char (ord)

newtype Json = Json { jval :: JValue }
             deriving (Show, Eq)

data JValue = Str String --
            | Num Double
            | Obj [(String, JValue)]
            | Arr [JValue]
            | Boolean Bool
            | Null
            deriving (Show, Eq)

whiteSpace :: Parser String
whiteSpace = many $ oneOf ['\n', ' ', '\t', '\r']

-- | Parser for JSON.
jsonParser :: Parser Json
jsonParser = do 
    output <- jsonCustomParser <* eof <|> unexpected "Invalid input"
    pure $ Json output

jsonCustomParser :: Parser JValue
jsonCustomParser = between whiteSpace whiteSpace $ choice [getNull, getBoolean, getString, getNumber, getList, getObject]
    where 
        getBoolean = Boolean True <$ string "true" <|> Boolean False <$ string "false"
        getNull = Null <$ string "null"
        getList = between (char '[' *> whiteSpace ) (char ']') $ fmap Arr $ sepBy jsonCustomParser $ char ',' *> whiteSpace
        getObject = between (char '{' *> whiteSpace ) (char '}') $ fmap Obj $ sepBy objectElement $ char ',' *> whiteSpace
          where
            objectElement = do
                Str key <- getString
                _ <- whiteSpace <* char ':' *> whiteSpace
                value <- jsonCustomParser
                return (key, value)
        getString = between (char '"') (char '"') $ fmap Str $ many $ char '\\' *> escapeChars <|> parseString
          where
            parseString = do 
                charX <- noneOf['"']
                if ord charX < 32
                    then    
                        unexpected "Invalid value."
                    else
                        pure charX
            escapeChars = do 
                characters <- oneOf ['"', '\\', '/', 'f', 'n', 'r', 'b', 't', 'u']
                case characters of
                    '"' -> return '"'
                    '\\' -> return '\\'
                    '/' -> return '/'
                    'f' -> return '\f'
                    'n' -> return '\n'
                    'r' -> return '\r'
                    'b' -> return '\b'
                    't' -> return '\t'
                    'u' -> do
                        hexDigits <- count 4 $ digit <|> oneOf ['a'..'f'] <|> oneOf ['A'..'F']
                        return (toEnum (read ("0x" ++ hexDigits) :: Int) :: Char)
                    _ -> unexpected "Invalid value."         
        getNumber = fmap Num parseNumber
          where
            parseNumber = do
                sign <- option "" $ string "-"
                firstNumber <- digit
                number <- many $ oneOf $ ['0'..'9'] ++ ['E', 'e', '+', '-', '.']
                if firstNumber == '0' && not (null number) && head number /= '.'
                    then
                        unexpected "Invalid value."
                    else 
                        parseError (readMaybe (sign ++ firstNumber : number) :: Maybe Double)
                  where
                    parseError Nothing = unexpected "Invalid input"
                    parseError (Just value) = return value
        
parseFile :: String -> IO ()
parseFile file = do
                 input <- readFile file
                 case runParser jsonParser () file input of
                      Left err   -> putStrLn $ "Error: " ++ show err
                      Right json -> putStrLn $ "Success:\n" ++ show json

main :: IO ()
main = do
       argv <- getArgs
       if length argv == 1 then parseFile $ head argv
                           else putStrLn "File to parse is missing!"