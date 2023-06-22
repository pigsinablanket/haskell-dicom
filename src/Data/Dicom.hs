module Data.Dicom where

import Control.Applicative
import Data.Binary
import Data.Binary.Get
import Data.Int
import Prelude hiding (LT)
import Text.Read
import qualified Data.ByteString.Lazy as B
import qualified Data.ByteString.Lazy.Char8 as C

data DicomElement = DicomElement
  { _tag :: Tag
  , _vr :: VR
  , _vl :: Int64
  , _value :: B.ByteString
  }
  deriving (Show)

reserved,unreserved :: [VR]
reserved = [OB, OW, OF, SQ, UT, UN]
unreserved = [AE, AS, AT, CS, DA, DS, DT, FL, FD, IS, LO, LT, PN, SH, SL, SS, ST, TM, UI, UL, US]

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

type Tag = (Group,Element)
type Group = Word16
type Element = Word16

readVR :: String -> Maybe VR
readVR = readMaybe

parseDicomElement :: Get DicomElement
parseDicomElement = do
  group <- getWord16le
  element <- getWord16le
  vr1 <- getWord8
  vr2 <- getWord8
  let vr = C.unpack $ B.pack [vr1,vr2]
  case readVR vr of
    Just vr' -> do
      vl <- if vr' `elem` unreserved
        then fromIntegral <$> getWord16le
        else fromIntegral <$> (skip 2 >> getWord32le)
      value <- getLazyByteString (vl)
      pure (DicomElement (group,element) vr' vl value)
    Nothing -> fail $  "Invalid VR: " ++ show vr

-- Parse the DICOM file meta information header
parseDicom :: Get [DicomElement] -- (Tag,VR,Int64,B.ByteString)
parseDicom = do
  skip 128
  prefix <- getLazyByteString 4

  if prefix /= (C.pack "DICM")
    then fail "Invalid DICOM file"
    else parseUntil isEmpty parseDicomElement

  --     transferSyntax <- getLazyByteString 20
  --     if transferSyntax == (C.pack "1.2.840.10008.1.2.1\NUL") -- Little-endian transfer syntax
  --       then pure "DICOM file is little endian"
  --       else if transferSyntax == (C.pack "1.2.840.10008.1.2.2\NUL") -- Big-endian transfer syntax
  --              then pure "DICOM file is big endian"
  --              else pure "Unknown transfer syntax"

readDicomFile :: FilePath -> IO B.ByteString
readDicomFile path = B.readFile path

parseDicomFile :: B.ByteString -> [DicomElement]
parseDicomFile bs =
  case runGetOrFail parseDicom bs of
    Left (_, _, err) -> error err
    Right (_, _, result) -> result

printDicom :: [DicomElement] -> IO ()
printDicom d = sequence_ $ printElem <$> d
  where
    printElem (DicomElement tag vr vl value) =
      print (tag,vr,vl, B.take 16 value)


main :: IO ()
main = do
  dicomFile <- readDicomFile "test/data/test.dcm"
  let dicomElements = parseDicomFile dicomFile
  print dicomElements

parseUntil :: Get Bool -> Get DicomElement -> Get [DicomElement]
parseUntil more element = do
  continue <- lookAhead more
  if continue
    then pure []
    else (:) <$> element <*> parseUntil more element
