{-# LANGUAGE NoMonomorphismRestriction, FlexibleContexts #-}

import Text.Parsec

-- super simple implementation
csvFile = line `endBy` eol
line = cell `sepBy` char ','
cell = many $ noneOf ",\n"
eol = char '\n'

parseCSV :: String -> Either ParseError [[String]]
parseCSV = parse csvFile "(unknown)"