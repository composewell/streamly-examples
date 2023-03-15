{-# LANGUAGE QuasiQuotes #-}
{-# OPTIONS_GHC -fspec-constr-recursive=4 #-}

import Data.Function ((&))
import Data.Functor.Identity (runIdentity)
import Streamly.Data.Parser (Parser)
import Streamly.Unicode.String (str)

import qualified Data.Char as Char
import qualified Streamly.Data.Fold as Fold
import qualified Streamly.Data.Parser as Parser
import qualified Streamly.Data.Stream as Stream

-- Example of a quoted log string to be parsed
quoted :: String
quoted = [str|
"[2023-02-04T09:15:20.549Z] \"GET /cards?customer_id=XXXXP/1.1\" 200
- \"-\" \"-\" 0 75 23 23 \"131.26.22.133,127.16.101.49,127.18.4.69\"
\"edge\" \"7ccc821a-03ff-49c1-a721-977e7cbd78f3\" \"api.example.in\"
\"127.18.55.25:1108\" outbound|443|v21945461|svc.cluster.local
127.18.44.67:75523 127.18.44.67:8443 127.18.4.69:14462
mum.example.net confirmtkt\n"
|]

-- Use double quote as the quoting char
isQuote :: Char -> Maybe Char
isQuote x =
    case x of
        '"' -> Just x
        _ -> Nothing

-- Transliterate \" and \n
tr :: Char -> Char -> Maybe Char
tr q x =
    case x of
        _ | x == q -> Just x
        'n' -> Just '\n'
        _ -> Nothing

-- quoted string parser, reads everything inside quotes as a single word
-- and processes the quotes and escapes to expose the words inside it.
parser :: Monad m => Parser Char m [Char]
parser = Parser.wordWithQuotes False tr '\\' isQuote Char.isSpace Fold.toList

main :: IO ()
main = do
    -- Strip outer quotes
    let res = runIdentity $ Stream.parse parser $ Stream.fromList quoted

    -- parse words inside quotes
    case res of
        Left err -> error $ "Malformed quoted string: " ++ show err
        Right unquoted ->
              Stream.fromList unquoted
            & Stream.parseMany parser
            & Stream.catRights
            & Stream.toList
            >>= print
