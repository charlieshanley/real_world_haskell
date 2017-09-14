{-# LANGUAGE NoMonomorphismRestriction, FlexibleContexts #-}

import Text.Parsec

csvFile :: Parsec String () [[String]]
csvFile = line `endBy` eol
line = cell `sepBy` char ','
cell = quotedCell <|> many (noneOf ",\n\r")

quotedCell = do
    char '"'
    content <- many quotedChar
    char '"' <?> "quote at end of cell"
    return content

quotedChar = noneOf "\"" <|> try (string "\"\"" >> return '"')

eol = try (string "\n\r")
    <|> try  (string "\r\n")
    <|> string "\n"
    <|> string "\r"
    <?> "end of line"

parseCSV :: String -> Either ParseError [[String]]
parseCSV = parse csvFile "(unknown)"


-- super simple implementation
csvFile' = line' `endBy` eol'
line' = cell' `sepBy` char ','
cell' = many $ noneOf ",\n"
eol' = char '\n'

parseCSV' :: String -> Either ParseError [[String]]
parseCSV' = parse csvFile' "(unknown)"