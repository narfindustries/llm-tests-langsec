{-# LANGUAGE GADTs #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE AllowAmbiguousTypes #-}

module NetworkTimeProtocolVersion4 where

import Daedalus.AST
import Daedalus.PP
import Daedalus.Type


data NTPVersion4 = NTPVersion4 {
  seconds :: Integer,
  fraction :: Integer
} deriving (Show, Generic, ToJSON, FromJSON)


instance Semigroup NTPVersion4 where
  (<>) (NTPVersion4 s1 f1) (NTPVersion4 s2 f2) = NTPVersion4 (s1 + s2) (f1 + f2)

instance Monoid NTPVersion4 where
  mempty = NTPVersion4 0 0


ntpVersion4Parser :: Parser NTPVersion4
ntpVersion4Parser = do
  seconds' <- integer
  fraction' <- integer
  return $ NTPVersion4 seconds' fraction'


main :: IO ()
main = do
  let p = ntpVersion4Parser
  print $ pp p
