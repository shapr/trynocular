{-# LANGUAGE LambdaCase #-}

module Demo where

import Data.ByteString (ByteString)
import Data.ByteString qualified as B
import Data.ByteString.Builder (Builder)
import Data.ByteString.Builder qualified as BB
import Data.ByteString.Lazy qualified as BL
import Data.Char (chr, ord)
import Data.Foldable (foldl')
import Data.Map.Strict qualified as M
import Data.Maybe (isJust)
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
import Text.Megaparsec.Byte qualified as MB

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

data RBTree a
  = Branch RB (RBTree a) a (RBTree a)
  | Nil
  deriving (Show, Eq, Ord, Generic)

data RB = Red | Black deriving (Show, Eq, Ord, Generic)

rbEmpty :: RBTree a
rbEmpty = Nil

rbInsert :: Ord a => a -> RBTree a -> RBTree a
rbInsert new t = blacken (go t)
  where
    go Nil = Branch Red Nil new Nil
    go (Branch c l y r) = case compare new y of
      LT -> balance c (go l) y r
      GT -> balance c l y (go r)
      EQ -> Branch c l y r

    blacken (Branch _ l y r) = Branch Black l y r
    blacken Nil = Nil

    balance Black (Branch Red (Branch Red a x b) y c) z d =
      Branch Red (Branch Black a x b) y (Branch Black c z d)
    balance Black (Branch Red a x (Branch Red b y c)) z d =
      Branch Red (Branch Black a x b) y (Branch Black c z d)
    balance Black a x (Branch Red (Branch Red b y c) z d) =
      Branch Red (Branch Black a x b) y (Branch Black c z d)
    balance Black a x (Branch Red b y (Branch Red c z d)) =
      Branch Red (Branch Black a x b) y (Branch Black c z d)
    balance c a x b = Branch c a x b

rbFromList :: Ord a => [a] -> RBTree a
rbFromList = foldl' (flip rbInsert) rbEmpty

rbMember :: Ord a => RBTree a -> a -> Bool
rbMember Nil _ = False
rbMember (Branch _ l y r) x = case compare x y of
  LT -> rbMember l x
  GT -> rbMember r x
  EQ -> True

isValid :: RBTree a -> Bool
isValid tree = rootIsBlack tree && noRedOnRed tree && isJust (blackLen tree)
  where
    rootIsBlack Nil = True
    rootIsBlack (Branch Black _ _ _) = True
    rootIsBlack _ = False

    noRedOnRed Nil = True
    noRedOnRed (Branch c l _ r) =
      (c == Black || (rootIsBlack l && rootIsBlack r))
        && noRedOnRed l
        && noRedOnRed r

    blackLen :: RBTree a -> Maybe Int
    blackLen Nil = Just 1
    blackLen (Branch c l _ r) =
      (if c == Black then succ else id)
        <$> (consistent (blackLen l) (blackLen r))

    consistent a b
      | a == b = a
      | otherwise = Nothing
