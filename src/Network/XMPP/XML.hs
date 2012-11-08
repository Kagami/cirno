module Network.XMPP.XML
    ( XMLElem(..)
    , getRest
    , deepTags
    , xmppStreamStart
    , xml2bytes
    , getAttr
    , getCData
    ) where

import Data.List (foldl')
import Data.Text (Text)
import Data.Text.Encoding (encodeUtf8)
import Data.Monoid ((<>))
import Data.ByteString (ByteString)
import Control.Applicative ((<*))
import Text.Parsec (getInput, try, satisfy, many, many1,
                    char, string, letter, space, (<|>))
import Text.Parsec.ByteString (Parser)
import qualified Data.Text as T
import qualified Data.ByteString as S

-- | A data structure representing an XML element.
data XMLElem
    -- | Tags have a name, a list of attributes, and a list of child
    -- elements.
    = XML Text [(Text, Text)] [XMLElem]

    -- | Character data just contains a string.
    | CData Text
    deriving (Show)

------------------------------
-- XML parsec parser
------------------------------

getRest :: Parser a -> Parser (a, ByteString)
getRest f = do
    x <- try f
    p <- getInput
    return (x, p)

deepTags :: Parser [XMLElem]
deepTags = many $ try deepTag

deepTag :: Parser XMLElem
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

shallowTag :: Parser XMLElem
shallowTag = tagStart <* char '>'

tagStart :: Parser XMLElem
tagStart = do
    char '<'
    name <- many1 tokenChar
    many1 space
    attrs <- many $ attribute <* many1 space
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

cdata :: Parser XMLElem
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

xmppStreamStart :: Parser XMLElem
xmppStreamStart = do
    try declaration
    shallowTag

declaration :: Parser ()
declaration = do
    char '<'
    char '?'
    many $ satisfy (/='?')
    char '?'
    char '>'
    return ()

------------------------------
-- XML serializer
------------------------------

-- | Convert the XML element back to bytes.
xml2bytes :: XMLElem -> ByteString
xml2bytes el = xml2bytes' [] [] [el]

xml2bytes' :: [ByteString] -> [ByteString] -> [XMLElem] -> ByteString
xml2bytes' acc1 acc2 [] =
    (S.concat $ reverse acc1) <> (S.concat acc2)
xml2bytes' acc1 acc2 ((CData text):els) =
    xml2bytes' acc1 ((escape text):acc2) els
xml2bytes' acc1 acc2 ((XML name attrs subels):els) =
    -- TODO: Could we get rid of slow (++) operator?
    xml2bytes' (open:acc1) acc2' (subels ++ els)
  where
    open = "<" <> enc name <> attrs2bytes attrs <> open'
    open'
      | null subels = "/>"
      | otherwise   = ">"
    acc2'
      | null subels = acc2
      | otherwise   = ("</" <> enc name <> ">"):acc2

-- | Convert list of attributes to bytestring.
attrs2bytes :: [(Text, Text)] -> ByteString
attrs2bytes = S.concat . reverse . (foldl' attrs2bytes' [])
  where
    attrs2bytes' acc attr = (attr2bytes attr):acc
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
getAttr :: Text -> XMLElem -> Maybe Text
getAttr attr (XML _ attrs _) = lookup attr attrs
getAttr _ _ = Nothing

-- | Get the character data subelement of the given tag.
getCData :: XMLElem -> Maybe Text
getCData (XML _ _ [CData text]) = Just text
getCData _ = Nothing

enc :: Text -> ByteString
enc = encodeUtf8
