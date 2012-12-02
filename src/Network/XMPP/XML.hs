{-# OPTIONS_GHC -fno-warn-unused-do-bind #-}

module Network.XMPP.XML
    ( XML(..)
    , parseStreamStart
    , parseTags
    , xml2bytes
    , tag2bytes
    , getAttr
    , getCData
    ) where

import Data.List (foldl')
import Data.Monoid ((<>))
import Data.Text (Text)
import Data.Text.Encoding (encodeUtf8)
import Data.ByteString (ByteString)
import Control.Applicative ((<*), (*>))
import Text.Parsec (parse, getInput, try, satisfy, many, many1,
                    char, string, letter, space, (<|>))
import Text.Parsec.Text (Parser)
import qualified Data.Text as T
import qualified Data.ByteString as S

-- | A data structure representing an XML element.
data XML
    -- | Tags have a name, a list of attributes, and a list of child
    -- elements.
    = XML Text [(Text, Text)] [XML]

    -- | Character data just contains a string.
    | CData Text
    deriving (Show)

------------------------------
-- XML parsec parser
------------------------------

parseStreamStart :: Text -> (XML, Text)
parseStreamStart = saxParse streamStart

parseTags :: Text -> ([XML], Text)
parseTags = saxParse deepTags

saxParse :: Parser a -> Text -> (a, Text)
saxParse parser input =
    case parse saxParse' "" input of
        Right result ->
            result
        Left err ->
            error $ "XML parser error: " ++ show err
  where
    saxParse' = do
        parsed <- try parser
        rest <- getInput
        return (parsed, rest)

streamStart :: Parser XML
streamStart = do
    try (declaration >> shallowTag)
    <|> shallowTag

declaration :: Parser ()
declaration = do
    string "<?"
    many $ satisfy (/='?')
    string "?>"
    return ()

shallowTag :: Parser XML
shallowTag = tagStart <* char '>'

deepTags :: Parser [XML]
deepTags = many $ try deepTag

deepTag :: Parser XML
deepTag = do
    XML name attrs _ <- tagStart
    subels <- try (string "/>" >> return [])
          <|> do
            char '>'
            els <- many $ (try deepTag) <|> cdata
            string "</"
            string $ T.unpack name
            char '>'
            return els
    return $ XML name attrs subels

tagStart :: Parser XML
tagStart = do
    char '<'
    name <- many1 tokenChar
    attrs <- many $ many1 space *> attribute
    many space
    return $ XML (T.pack name) attrs []

tokenChar :: Parser Char
tokenChar = letter <|> char ':' <|> char '-'

attribute :: Parser (Text, Text)
attribute = do
    name <- many1 tokenChar
    char '='
    quote <- char '\'' <|> char '"'
    value <- many $ satisfy (/=quote)
    char quote
    return (T.pack name, T.pack value)

cdata :: Parser XML
cdata = do
    text <- many1 $ plainCdata <|> predefinedEntity
    return $ CData $ T.pack text
  where
    plainCdata = satisfy (\c -> c /= '<' && c /= '&')
    predefinedEntity = do
        char '&'
        entity <- try (string "amp"  >> return '&')
              <|> try (string "lt"   >> return '<')
              <|> try (string "gt"   >> return '>')
              <|> try (string "apos" >> return '\'')
              <|> try (string "quot" >> return '"')
        char ';'
        return entity

------------------------------
-- XML serializers
------------------------------

-- | Convert the XML element back to bytes.
xml2bytes :: XML -> ByteString
xml2bytes el = xml2bytes' [] [] [el]

xml2bytes' :: [ByteString] -> [ByteString] -> [XML] -> ByteString
xml2bytes' acc1 acc2 [] =
    (S.concat $ reverse acc1) <> (S.concat acc2)
xml2bytes' acc1 acc2 ((CData text):els) =
    xml2bytes' acc1 ((escape text):acc2) els
xml2bytes' acc1 acc2 ((XML name attrs subels):els) =
    -- TODO: Could we throw out slow (++) operator?
    xml2bytes' (open:acc1) acc2' (subels ++ els)
  where
    open = "<" <> enc name <> attrs2bytes attrs <> open'
    open'
      | null subels = "/>"
      | otherwise   = ">"
    acc2'
      | null subels = acc2
      | otherwise   = ("</" <> enc name <> ">"):acc2

-- | Serialize only first XML tag.
tag2bytes :: XML -> ByteString
tag2bytes (CData text) =
    enc text
tag2bytes (XML name attrs _) =
    "<" <> enc name <> attrs2bytes attrs <> ">"

-- | Convert list of attributes to bytestring.
attrs2bytes :: [(Text, Text)] -> ByteString
attrs2bytes = S.concat . reverse . (foldl' attrs2bytes' [])
  where
    -- Fold function.
    attrs2bytes' acc attr = (attr2bytes attr):acc
    -- Serialize just one key-value pair.
    attr2bytes (name, value) = " " <> enc name <> "='" <> enc value <> "'"

-- | Escape special XML characters.
escape :: Text -> ByteString
escape = S.concat . reverse . (T.foldl' escape' [])
  where
    escape' acc ch = (char2bytes ch):acc
    char2bytes ch = case ch of
        '&'  -> "&amp;"
        '<'  -> "&lt;"
        '>'  -> "&gt;"
        '"'  -> "&quot;"
        '\'' -> "&apos;"
        _    -> enc $ T.singleton ch

------------------------------
-- Helper functions
------------------------------

-- | Get the value of an attribute in the given tag.
getAttr :: Text -> XML -> Maybe Text
getAttr attr (XML _ attrs _) = lookup attr attrs
getAttr _ _ = Nothing

-- | Get the character data subelement of the given tag.
getCData :: XML -> Maybe Text
getCData (XML _ _ [CData text]) = Just text
getCData _ = Nothing

enc :: Text -> ByteString
enc = encodeUtf8
