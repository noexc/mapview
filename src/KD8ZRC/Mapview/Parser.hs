module KD8ZRC.Mapview.Parser where

import Control.Applicative
import qualified Data.Text as T
import Data.Thyme.Format
import Linear.V3
import System.Locale
import Text.Trifecta

import KD8ZRC.Mapview.Types

-- | Input is in the following format:
--
-- >>> :W8UPD:41.09347:-80.74683:237.0:032553:656:-40:83:
parseLine :: Parser (IO RTTYLine)
parseLine = do
  _ <- colon
  callsign' <- manyTill anyChar (try colon)

  lat' <- eitherToNum <$> integerOrDouble
  _ <- colon

  lon' <- eitherToNum <$> integerOrDouble
  _ <- colon

  altitude' <- eitherToNum <$> integerOrDouble
  _ <- colon

  time' <- many (token digit)
  _ <- colon

  magX <- integer
  _ <- colon

  magY <- integer
  _ <- colon

  magZ <- integer
  _ <- colon

  celsius <- eitherToNum <$> integerOrDouble
  _ <- colon

  return $ return $ RTTYLine
    (T.pack callsign')
    (Coordinates lat' lon')
    altitude'
    (readTime defaultTimeLocale "%H%M%S" time')
    (MagField (V3 magX magY magZ))
    (Celsius celsius)

eitherToNum :: (Num b, Integral a) => Either a b -> b
eitherToNum = either fromIntegral id
