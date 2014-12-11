module System.Console.ANSI.Code (Code(..), encode1, encode, decode1, decode) where

import Text.ParserCombinators.ReadP

data Code = CharCode Char
          | C1 Char
          | CS String String Char
          deriving (Eq, Ord)

-- Show/Read using String
-- Compact, but data can be lost with malformed sequences
-- Use deriving() instead?
instance Show Code where
    show = show . encode1
    showList = showList . encode

instance Read Code where
    readsPrec p = concat . map (\(v, s) -> map (\x -> (fst x, s)) $ readP_to_S (code >>= \x -> eof >> return x) v) . readsPrec p
    readList = concat . map (\(v, s) -> map (\x -> (fst x, s)) $ readP_to_S (many code >>= \x -> eof >> return x) v) . readList

encode1 :: Code -> String
encode1 (CharCode c) = [c]
encode1 (C1 c) = '\ESC' : [c]
encode1 (CS p i f) = '\ESC' : '[' : p ++ i ++ [f]

encode :: [Code] -> String
encode = concatMap encode1

code :: ReadP Code
code = controlSeq <++ control1 <++ fmap CharCode get
     where esc = char '\ESC'
           csi = control1 >>= \c -> if c == C1 '[' then return c else pfail -- TODO: allow \CSI for UTF/ASCII-8
           control1 = esc >> (fmap C1 $ satisfy (incRange '@' '_'))
           controlSeq = do _ <- csi
                           p <- parameter
                           i <- intermediate
                           f <- satisfy $ incRange '@' '~'
                           return $ CS p i f
           parameter = munch $ incRange '0' '?'
           intermediate = munch $ incRange ' ' '/'
           incRange s e c = c >= s && c <= e

decode1 :: String -> (Code, String)
decode1 = head . readP_to_S code

decode :: String -> [Code]
decode [] = []
decode s = let (v, n) = decode1 s
           in v : decode n
