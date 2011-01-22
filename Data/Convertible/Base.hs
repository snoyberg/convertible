{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TemplateHaskell #-}
{-
Copyright (C) 2009 John Goerzen <jgoerzen@complete.org>

All rights reserved.

For license and copyright information, see the file LICENSE

-}

{- |
   Module     : Data.Convertible.Base
   Copyright  : Copyright (C) 2009 John Goerzen
   License    : BSD3

   Maintainer : Michael Snoyman <michael@snoyman.com>
   Stability  : provisional
   Portability: portable

-}

module Data.Convertible.Base( ConvertAttempt (..),
                              failure,
                              ConvertSuccess (..),
                              cs,
                              ca,
                              ConversionException (..),
                              convertUnsafe,
                              convertAttemptWrap,
                              deriveAttempts
                             )
where
import Data.Attempt
import Control.Exception (Exception)
import Data.Typeable (Typeable)
import Language.Haskell.TH

----------------------------------------------------------------------
-- Conversions
----------------------------------------------------------------------

{- | A typeclass that represents something that can attempt a conversion.
An @ConvertAttempt a b@ instance represents an @a@ that might be convertible to a @b@. -}
class ConvertAttempt a b where
    {- | Convert @a@ to @b@, returning 'Success' on success and 'Failure' on error.
     -}
    convertAttempt :: a -> Attempt b

-- | A convenience synonym for 'convertAttempt'
ca :: ConvertAttempt x y => x -> Attempt y
ca = convertAttempt

{- | A typeclass that represents something that guarantees a successful conversion.
A @ConvertSuccess a b@ instance represents an @a@ that can be converted to a @b@. -}
class ConvertSuccess a b where
    {- | Convert @a@ to @b@. -}
    convertSuccess :: a -> b

-- | A convenience synonym for 'convertSuccess'
cs :: ConvertSuccess x y => x -> y
cs = convertSuccess

{- | Any type can be converted to itself. -}
instance ConvertSuccess a a where
    convertSuccess = id
instance ConvertAttempt a a where
    convertAttempt = return

{- FIXME consider exposing this
{- | Lists of any convertible type can be converted. -}
instance Convertible a b => Convertible [a] [b] where
    safeConvert = mapM safeConvert
-}

{- | Convert from one type of data to another.  Raises an exception if there is
an error with the conversion.  For a function that does not raise an exception
in that case, see 'convertAttempt'. -}
convertUnsafe :: ConvertAttempt a b => a -> b
convertUnsafe = fromSuccess . convertAttempt

{- | Wraps any 'Exception' which could occur during a 'convertAttempt'.
-}
data ConversionException = forall e. Exception e => ConversionException e
    deriving Typeable
instance Show ConversionException where
    show (ConversionException e) = "ConversionException " ++ show e
instance Exception ConversionException

{- | Calls 'convertAttempt', wrapping any 'Exception's in a
 'ConversionException'
-}
convertAttemptWrap :: (ConvertAttempt a b,
                       Failure ConversionException m
                      )
                   => a
                   -> m b
convertAttemptWrap = attempt (failure . ConversionException) return .
                     convertAttempt

convertAttempt' :: ConvertSuccess x y => x -> Attempt y
convertAttempt' = return . convertSuccess

-- | Template Haskell to derive 'ConvertAttempt' instances from the
-- corresponding 'ConvertSuccess' instances.
deriveAttempts :: [(Name, Name)] -> Q [Dec]
deriveAttempts pairs = do
    ca' <- [|convertAttempt'|]
    return $ map (helper ca') pairs
      where
        helper ca' (x, y) =
            InstanceD
                []
                (ConT (mkName "ConvertAttempt") `AppT` ConT x `AppT` ConT y)
                [ FunD (mkName "convertAttempt")
                    [ Clause [] (NormalB ca') []
                    ]
                ]
