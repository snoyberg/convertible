{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
---------------------------------------------------------
--
-- Module        : Data.Convertible.Instances.String
-- Copyright     : Michael Snoyman
-- License       : BSD3
--
-- Maintainer    : Michael Snoyman <michael@snoyman.com>
-- Stability     : Stable
-- Portability   : portable
--
---------------------------------------------------------

-- | Instances of 'ConvertSuccess' and 'ConvertAttempt' for 'String', along
-- with instances for bytestrings and text (lazy and strict).
module Data.Convertible.Instances.String
    ( InvalidDayException (..)
    , InvalidBoolException (..)
#if TEST
    , testSuite
#endif
    ) where

import Data.Convertible.Base
import Data.Typeable (Typeable)
import Control.Exception (Exception)
import qualified Safe.Failure as SF
import Data.Convertible.Instances.Text ()
import Data.Attempt
import Control.Monad ((<=<))

import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BL
import qualified Data.Text as ST
import qualified Data.Text.Lazy as LT

import Data.Time.Calendar
import Data.Ratio

#if TEST
import Test.Framework (testGroup, Test)
--import Test.Framework.Providers.HUnit
import Test.Framework.Providers.QuickCheck2 (testProperty)
--import Test.HUnit hiding (Test, path)
import Data.Char (isDigit)
#endif

$(deriveAttempts
    [ (''Bool, ''BL.ByteString)
    , (''Bool, ''BS.ByteString)
    , (''Bool, ''String)
    , (''Bool, ''LT.Text)
    , (''Bool, ''ST.Text)
    , (''Day, ''BL.ByteString)
    , (''Day, ''BS.ByteString)
    , (''Day, ''String)
    , (''Day, ''LT.Text)
    , (''Day, ''ST.Text)
    , (''Int, ''BL.ByteString)
    , (''Int, ''BS.ByteString)
    , (''Int, ''String)
    , (''Int, ''LT.Text)
    , (''Int, ''ST.Text)
    , (''Rational, ''BL.ByteString)
    , (''Rational, ''BS.ByteString)
    , (''Rational, ''String)
    , (''Rational, ''LT.Text)
    , (''Rational, ''ST.Text)
    ])

{- Not needed yet
fromString :: ConvertSuccess String a => String -> a
fromString = convertSuccess
-}

fromStringA :: ConvertAttempt String a => String -> Attempt a
fromStringA = convertAttempt

toString :: ConvertSuccess a String => a -> String
toString = convertSuccess

-- Day
data InvalidDayException = InvalidDayException String
    deriving (Show, Typeable)
instance Exception InvalidDayException

instance ConvertSuccess Day [Char] where
    convertSuccess = show
instance ConvertAttempt [Char] Day where
    convertAttempt s = wrapFailure (const $ InvalidDayException s) $ do
        SF.assert (length s == 10) () $ InvalidDayException s
        y <- SF.read $ take 4 s
        m <- SF.read $ take 2 $ drop 5 s
        d <- SF.read $ take 2 $ drop 8 s
        return $ fromGregorian y m d

-- Bool
data InvalidBoolException = InvalidBoolException String
    deriving (Show, Typeable)
instance Exception InvalidBoolException

instance ConvertSuccess Bool [Char] where
    convertSuccess b = if b then "true" else "false"
instance ConvertAttempt [Char] Bool where
    convertAttempt s =
        case s of
            -- list comes from http://yaml.org/type/bool.html
            "y" -> return True
            "Y" -> return True
            "yes" -> return True
            "Yes" -> return True
            "YES" -> return True
            "true" -> return True
            "True" -> return True
            "TRUE" -> return True
            "on" -> return True
            "On" -> return True
            "ON" -> return True

            "n" -> return False
            "N" -> return False
            "no" -> return False
            "No" -> return False
            "NO" -> return False
            "false" -> return False
            "False" -> return False
            "FALSE" -> return False
            "off" -> return False
            "Off" -> return False
            "OFF" -> return False

            _ -> failure $ InvalidBoolException s

-- Int
instance ConvertSuccess Int [Char] where
    convertSuccess = show
instance ConvertAttempt [Char] Int where
    convertAttempt = SF.read

-- Rational
instance ConvertSuccess Rational [Char] where
    convertSuccess r
        | denominator r == 1 = show $ numerator r
        | otherwise = show $ (fromRational r :: Double)
instance ConvertAttempt [Char] Rational where
    convertAttempt = fmap realToFrac . (SF.read :: String -> Attempt Double)

#if TEST
propRationalId :: Rational -> Bool
propRationalId r = convertAttempt (convertSuccess r :: String) `almostEquals` r

almostEquals :: Attempt Rational -> Rational -> Bool
almostEquals (Failure _) _ = False
almostEquals (Success x) y = abs (x - y) < 0.0001

propRationalAllDigits :: Rational -> Bool
propRationalAllDigits = all (\c -> isDigit c || c `elem` ".-e")
                      . convertSuccess
#endif

-- Instances for bytestrings and text (generated by utils/StringHelper.hs)

instance ConvertAttempt BS.ByteString Day where
    convertAttempt = fromStringA <=< convertAttempt
instance ConvertAttempt BL.ByteString Day where
    convertAttempt = fromStringA <=< convertAttempt
instance ConvertAttempt ST.Text Day where
    convertAttempt = fromStringA <=< convertAttempt
instance ConvertAttempt LT.Text Day where
    convertAttempt = fromStringA <=< convertAttempt
instance ConvertAttempt BS.ByteString Bool where
    convertAttempt = fromStringA <=< convertAttempt
instance ConvertAttempt BL.ByteString Bool where
    convertAttempt = fromStringA <=< convertAttempt
instance ConvertAttempt ST.Text Bool where
    convertAttempt = fromStringA <=< convertAttempt
instance ConvertAttempt LT.Text Bool where
    convertAttempt = fromStringA <=< convertAttempt
instance ConvertAttempt BS.ByteString Int where
    convertAttempt = fromStringA <=< convertAttempt
instance ConvertAttempt BL.ByteString Int where
    convertAttempt = fromStringA <=< convertAttempt
instance ConvertAttempt ST.Text Int where
    convertAttempt = fromStringA <=< convertAttempt
instance ConvertAttempt LT.Text Int where
    convertAttempt = fromStringA <=< convertAttempt
instance ConvertAttempt BS.ByteString Rational where
    convertAttempt = fromStringA <=< convertAttempt
instance ConvertAttempt BL.ByteString Rational where
    convertAttempt = fromStringA <=< convertAttempt
instance ConvertAttempt ST.Text Rational where
    convertAttempt = fromStringA <=< convertAttempt
instance ConvertAttempt LT.Text Rational where
    convertAttempt = fromStringA <=< convertAttempt
instance ConvertSuccess Day BS.ByteString where
    convertSuccess = convertSuccess . toString
instance ConvertSuccess Day BL.ByteString where
    convertSuccess = convertSuccess . toString
instance ConvertSuccess Day ST.Text where
    convertSuccess = convertSuccess . toString
instance ConvertSuccess Day LT.Text where
    convertSuccess = convertSuccess . toString
instance ConvertSuccess Bool BS.ByteString where
    convertSuccess = convertSuccess . toString
instance ConvertSuccess Bool BL.ByteString where
    convertSuccess = convertSuccess . toString
instance ConvertSuccess Bool ST.Text where
    convertSuccess = convertSuccess . toString
instance ConvertSuccess Bool LT.Text where
    convertSuccess = convertSuccess . toString
instance ConvertSuccess Int BS.ByteString where
    convertSuccess = convertSuccess . toString
instance ConvertSuccess Int BL.ByteString where
    convertSuccess = convertSuccess . toString
instance ConvertSuccess Int ST.Text where
    convertSuccess = convertSuccess . toString
instance ConvertSuccess Int LT.Text where
    convertSuccess = convertSuccess . toString
instance ConvertSuccess Rational BS.ByteString where
    convertSuccess = convertSuccess . toString
instance ConvertSuccess Rational BL.ByteString where
    convertSuccess = convertSuccess . toString
instance ConvertSuccess Rational ST.Text where
    convertSuccess = convertSuccess . toString
instance ConvertSuccess Rational LT.Text where
    convertSuccess = convertSuccess . toString

#if TEST
testSuite :: Test
testSuite = testGroup "Data.Convertible.Instances.String"
    [ testProperty "propRationalId" propRationalId
    , testProperty "propRationalAllDigits" propRationalAllDigits
    , testProperty "propRationalAllNums" propRationalAllNums
    ]

propRationalAllNums :: Integer -> Bool
propRationalAllNums i = go (i % 1) where
  go :: Rational -> Bool
  go r
    | r < 0 = go $ negate r
    | otherwise = all isDigit $ cs r

#endif
