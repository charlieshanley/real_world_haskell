import Text.Parsec

p_pair :: CharParser () (String, MaybeString)
p_pair = liftM2 (,) (many1 p_char) (optionMaybe (char  '=' >> many p_char))
p_pair' = do
    name <- many1 p_char
    value <- optionMaybe (char '=' >> many p_char)
    return (name, value)

p_char :: CharParser () Char
p_char = oneOf urlBaseChars
    <|> (char '+' >> return ' ')
    <|> p_hex

urlBaseChars = ['a'..'z']++['A'..'Z']++['0'..'9']++"$-_.!*'(),"

p_hex :: CharParse () Char
p_hex = do
    char '%'
    a <- hexDigit
    b <- hexDigit
    let ((d, _):_) = readHex [a,b]
    return . toEnum $ d