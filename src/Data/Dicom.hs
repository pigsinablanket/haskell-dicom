{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE MultiWayIf #-}

module Data.Dicom where

import Control.Monad.Reader (ReaderT, runReaderT, ask)
import Control.Monad.Trans.Class (lift)
import Data.Binary qualified as Bin
import Data.Binary.Get qualified as Bin
import Data.Word (Word8, Word16, Word32)
import Data.Int (Int64)
import Data.List (find)
import Numeric (showHex)
import Prelude hiding (LT)
import Text.Read (readMaybe)
import qualified Data.ByteString.Lazy as B
import qualified Data.ByteString.Lazy.Char8 as C

type DataSet = [DicomElement]

data Tag = Tag Word16 Word16
  deriving (Eq, Show)

data VR
  = AE
  | AS
  | AT
  | CS
  | DA
  | DS
  | DT
  | FD
  | FL
  | IS
  | LO
  | LT
  | OB
  | OF
  | OW
  | PN
  | SH
  | SL
  | SQ
  | SS
  | ST
  | TM
  | UI
  | UL
  | UN
  | US
  | UT
  deriving (Eq, Read, Show)

data DicomElement = DicomElement
  { elementTag :: Tag
  , elementVR :: VR
  , elementVL :: Int64
  , elementValue :: B.ByteString
  }
  deriving (Eq, Show)

data Endianness = BigEndian | LittleEndian

class Monad m => MonadEndianness m where
  skip :: Int -> m ()
  getLazyByteString :: Int64 -> m B.ByteString
  getWord16 :: m Word16
  getWord32 :: m Word32
  parseUntil :: Bin.Get Bool -> m a -> m [a]
  getWord8 :: m Word8

instance MonadEndianness (ReaderT Endianness Bin.Get) where
  skip = lift . Bin.skip
  getLazyByteString = lift . Bin.getLazyByteString
  getWord8 = lift Bin.getWord8
  getWord16 = ask >>= \case
    BigEndian    -> lift Bin.getWord16be
    LittleEndian -> lift Bin.getWord16le
  getWord32 = ask >>= \case
    BigEndian    -> lift Bin.getWord32be
    LittleEndian -> lift Bin.getWord32le
  parseUntil more element = do
    continue <- lift (Bin.lookAhead more)
    if continue
      then pure []
      else (:) <$> element <*> parseUntil more element

reserved,unreserved :: [VR]
reserved = [OB, OW, OF, SQ, UT, UN]
unreserved = [AE, AS, AT, CS, DA, DS, DT, FL, FD, IS, LO, LT, PN, SH, SL, SS, ST, TM, UI, UL, US]

readVR :: String -> Maybe VR
readVR = readMaybe

parseDicomElement :: (MonadEndianness m, MonadFail m) =>  m DicomElement
parseDicomElement = do
  group <- getWord16
  element <- getWord16
  vr1 <- getWord8
  vr2 <- getWord8
  let vr = C.unpack $ B.pack [vr1,vr2]
  case readVR vr of
    Just vr' -> do
      vl <- if vr' `elem` unreserved
        then fromIntegral <$> getWord16
        else fromIntegral <$> (skip 2 >> getWord32)
      value <- getLazyByteString (vl)
      pure (DicomElement (Tag group element) vr' vl value)
    Nothing -> fail $  "Invalid VR: " ++ show vr

parseDicomMeta :: (MonadEndianness m, MonadFail m) =>  m [DicomElement]
parseDicomMeta = parseUntil isNotGroup2 parseDicomElement
  where
    isNotGroup2 = Bin.lookAhead (Bin.getWord16le >>= pure . (/=) 2)

parseDicomBody :: (MonadEndianness m, MonadFail m) =>  m [DicomElement]
parseDicomBody = parseUntil (Bin.isEmpty) parseDicomElement

parseDicomHeader :: (MonadEndianness m, MonadFail m) =>  m ()
parseDicomHeader = do
  skip 128
  prefix <- getLazyByteString 4
  if prefix /= (C.pack "DICM")
    then fail "Invalid DICOM file"
    else pure ()

lookupElementByTag :: Tag -> [DicomElement] -> Maybe DicomElement
lookupElementByTag tag elements = find (\(DicomElement tag' _ _ _) -> tag == tag') elements

parseLittleEndian :: ReaderT Endianness m a -> m a
parseLittleEndian = flip runReaderT LittleEndian

parseBigEndian :: ReaderT Endianness m a -> m a
parseBigEndian = flip runReaderT BigEndian

readDicomFile :: FilePath -> IO B.ByteString
readDicomFile path = B.readFile path

parseDicom :: Bin.Get [DicomElement]
parseDicom = do
  _ <- parseLittleEndian parseDicomHeader
  meta <- parseLittleEndian parseDicomMeta
  body <- determineEndian meta >>= \case
    LittleEndian -> parseLittleEndian parseDicomBody
    BigEndian -> parseBigEndian parseDicomBody
  pure $ meta++body
  where
    determineEndian meta =
      case lookupElementByTag (Tag 0x2 0x010) meta of
        Just (DicomElement _ _ _ value) ->
          if | (B.filter (/= 0x00) value) == (C.pack "1.2.840.10008.1.2.1") -> pure LittleEndian
             | (B.filter (/= 0x00) value) == (C.pack "1.2.840.10008.1.2.2") -> pure BigEndian
             | otherwise -> fail $ "Unknown transfer syntax: " ++ show value
        Nothing -> fail "No transfer syntax specified"

parseDicomFile :: B.ByteString -> [DicomElement]
parseDicomFile bs =
  case Bin.runGetOrFail parseDicom bs of
    Left (_, _, err) -> error err
    Right (_, _, result) -> result

printDicom :: [DicomElement] -> IO ()
printDicom d = sequence_ $ printElem <$> d
  where
    printElem (DicomElement (Tag g e) vr vl value) =
      print ((showWord16Hex g, showWord16Hex e),vr,vl, B.take 32 value)

    showWord16Hex :: Word16 -> String
    showWord16Hex n = "0x" ++ showHex n ""

main :: IO ()
main = do
  dicomFile <- readDicomFile "test/data/test.dcm"
  let dicomElements = parseDicomFile dicomFile
  printDicom dicomElements
