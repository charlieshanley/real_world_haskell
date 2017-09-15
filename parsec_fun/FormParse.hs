{-# LANGUAGE NoMonomorphismRestriction, FlexibleContexts #-}

-- import Text.Parsec
import Text.ParserCombinators.Parsec
import Control.Monad
import Control.Applicative (liftA2)
import Numeric (readHex)

p_pair :: CharParser () (String, Maybe String)
p_pair = liftA2 (,) (many1 p_char) (optionMaybe (char '=' *> many p_char))

p_char :: CharParser () Char
p_char = oneOf urlBaseChars <|> (' ' <$ char '+') <|> p_hex

urlBaseChars = ['a'..'z']++['A'..'Z']++['0'..'9']++"$-_.!*'(),"

p_hex :: CharParser () Char
p_hex = hexify <$> (char '%' *> hexDigit) <*> hexDigit

hexify :: Char -> Char -> Char
hexify a b = toEnum . fst . head . readHex $ [a, b]

p_hex' = do
    char '%'
    a <- hexDigit
    b <- hexDigit
    let ((d, _):_) = readHex [a,b]
    return . toEnum $ d