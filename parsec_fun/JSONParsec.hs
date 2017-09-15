{-# LANGUAGE NoMonomorphismRestriction, FlexibleContexts #-}

import Text.ParserCombinators.Parsec
import Control.Applicative ((*>))

import SimpleJSON

p_text :: CharParser () JValue
p_text = spaces *> text <?> "JSON text"
    where text = JObject <$> p_object <|> JArray <$> p_array

p_series :: Char -> CharParser () a -> Char -> CharParser () [a]
p_series left parser right = between (char left <* spaces) (char right) $
    (parser <* spaces) `sepBy` (char ',' <* spaces)

p_array = JAry <$> p_series '[' p_value ']'

p_object = JObj <$> p_series '{' p_field '}'
    where p_field = (,) <$> (p_string <* char ':' <* spaces) <*> p_value

p_value = value <* spaces
    where value = JString <$> p_string
              <|> JNumber <$> p_number
              <|> JObject <$> p_object
              <|> JArray <$> p_array
              <|> JNull <$> string "null"
              <?> "JSON value"


p_bool = True <$ string "true" <|> False <$ string "false"

p_value_choice = vale <* spaces
    where value = choice [ JString <$> p_string
                         , JNumber <$> p_number
                         , JObject <$> p_object
                         , JArray <$> p_array
                         , JBool <$> p_bool
                         , JNull <$ string "null"
                         ]
                  <?> "JSON value"

p_number :: CharParser () Double
p_number = do
    s <- getInput
    case readSigned realFloat s of
        [(n, s')] -> n <$ setInput s'
        _         -> empty


p_string :: CharParser () String
p_string = between (char '\"') (char '\"') (many jchar)
    where jchar = char '\\' *> (p_escape <|> p_unicode)
              <|> satifsy (`notElem` "\"\\")

p_escape = choice (zipWith decode "bnfrt\\\"/" "\b\n\f\r\t\\\"/")
    where decode c r = r <$ char c

p_unicode = CharParser () Char
p_unicode = char 'u' *> (decode <$> count 4 hexDigit)
    where decode x = toEnum code
        where ((code,_):_) = readHex x