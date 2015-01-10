{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}

module Main where

import Test.Tasty
import Test.Tasty.QuickCheck as QC
import Test.QuickCheck.Monadic (assert, monadicIO, run)

import Control.Applicative
import qualified Data.ByteString as BR
import qualified Data.ByteString.Builder as BB
import qualified Data.ByteString.Lazy.Char8 as B
import Data.Digest.CRC16
import Data.Typeable
import Data.Word
import Foreign.C.String
import Foreign.C.Types
import GHC.Generics

newtype ITA2String = ITA2String String deriving (Eq, Generic, Ord, Show, Typeable)

instance Arbitrary ITA2String where
  arbitrary = ita2String
    where
      ita2Char = oneof (map return "QWERTYUIOPASDFGHJKLZXCVBNM\r\n 1234567890-!&#'()\"/:;?,.")
      ita2String = ITA2String <$> listOf ita2Char

foreign import ccall "crc16.h crc16" c_crc16 :: CString -> CInt -> CInt

crcReference :: ITA2String -> IO Word16
crcReference (ITA2String s) = (\x -> fromIntegral $ c_crc16 x (fromIntegral . length $ s)) <$> newCString s

crcHaskellF :: Word16 -> Bool -> Word16 -> [Word8] -> Word16
crcHaskellF poly inverse initial = BR.foldl (crc16Update poly inverse) initial . BR.pack

crcHaskell :: ITA2String -> IO Word16
crcHaskell (ITA2String s) =
  return $
  crcHaskellF 0x1021 False 0xffff [fromIntegral (fromEnum x) :: Word8 | x <- s]

toHex :: Word16 -> B.ByteString
toHex n = BB.toLazyByteString . BB.word16Hex $ n

prop_hask_c_crc16 :: ITA2String -> Property
prop_hask_c_crc16 s = monadicIO $ do
  c <- run (toHex <$> crcReference s)
  hask <- run (toHex <$> crcHaskell s)
  assert $ c == hask

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "Tests" [properties]

properties :: TestTree
properties = testGroup "Properties" [qcProps]

qcProps :: TestTree
qcProps = testGroup "(checked by QuickCheck)"
  [ QC.testProperty "crcReference === crcHaskell ∀ ita2 strings" prop_hask_c_crc16
  ]
