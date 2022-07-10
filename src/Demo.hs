{-# LANGUAGE LambdaCase #-}

module Demo where

import Data.ByteString (ByteString)
import qualified Data.ByteString as B
import Data.ByteString.Builder (Builder)
import qualified Data.ByteString.Builder as BB
import qualified Data.ByteString.Lazy as BL
import Data.Char (chr, ord)
import qualified Data.Map.Strict as M
import Data.Void (Void)
import Data.Word (Word8)
import GHC.Generics (Generic)
import Text.Megaparsec
  ( MonadParsec (takeP, takeWhileP, try),
    Parsec,
    manyTill,
    optional,
    parse,
    satisfy,
    (<|>),
  )
import qualified Text.Megaparsec.Byte as MB

type Parser = Parsec Void ByteString

-- "i10e s3abce"
data Bencode
  = BString ByteString
  | BInteger Integer
  | BList [Bencode]
  | BDict (M.Map ByteString Bencode)
  deriving (Show, Eq, Ord, Generic)

bencode :: Parser Bencode
bencode =
  BString <$> try bString
    <|> BInteger <$> try bInteger
    <|> BList <$> try bList
    <|> BDict <$> bDict

prop_roundtrip :: Bencode -> Bool
prop_roundtrip benc = do
  parsed == Right benc
  where
    bs = BL.toStrict . BB.toLazyByteString $ write benc
    parsed = parse bencode "" bs

-- 3:abc -> "abc"
bString :: Parser ByteString
bString = do
  len <- number
  _ <- char_ ':'
  takeP Nothing (fromIntegral len)

-- i10e -> 10
bInteger :: Parser Integer
bInteger = do
  _ <- char_ 'i'
  i <-
    optional (char_ '-') >>= \case
      Nothing -> number
      Just _ -> negate <$> positiveNumber
  _ <- char_ 'e'
  pure i

number :: Parser Integer
number = try positiveNumber <|> (0 <$ char_ '0')

positiveNumber :: Parser Integer
positiveNumber = do
  d1 <- satisfy isNonZeroDigit
  ds <- takeWhileP Nothing isDigit
  let i = read @Integer $ map (chr . fromIntegral) (d1 : B.unpack ds)
  pure i

isDigit :: Word8 -> Bool
isDigit x = x >= 48 && x <= 57

isNonZeroDigit :: Word8 -> Bool
isNonZeroDigit x = x >= 49 && x <= 57

bList :: Parser [Bencode]
bList = do
  _ <- char_ 'l'
  manyTill bencode (char_ 'e')

bDict :: Parser (M.Map ByteString Bencode)
bDict = do
  _ <- char_ 'd'
  M.fromList <$> manyTill ((,) <$> bString <*> bencode) (char_ 'e')

char_ :: Char -> Parser Word8
char_ c = MB.char $ fromIntegral $ ord c

delimit :: Char -> Builder -> Builder
delimit d bs = BB.char8 d <> bs <> BB.char8 'e'

write :: Bencode -> Builder
write (BString bs) = (BB.intDec . B.length $ bs) <> BB.char8 ':' <> BB.byteString bs
write (BInteger i) = delimit 'i' $ BB.integerDec i
write (BList bs) = delimit 'l' $ foldMap write bs
write (BDict m) = delimit 'd' $ M.foldMapWithKey (\k v -> write (BString k) <> write v) m
