{-# Language DataKinds #-}
{-# Language KindSignatures #-}
{-# Language TypeOperators #-}
{-# Language MultiParamTypeClasses #-}
{-# Language FlexibleInstances #-}
{-# Language StandaloneDeriving #-}
{-# Language ScopedTypeVariables #-}
{-# Language FlexibleContexts #-}
{-# Language AllowAmbiguousTypes #-}
{-# Language OverloadedStrings #-}
{-# Language TypeApplications #-}
{-# Language TypeFamilies #-}
{-# Language ViewPatterns #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}
module Hl7-v2-gemini-1.5-flash where
 
import qualified RTS.Parser as RTS
import qualified Prelude as HS
import qualified GHC.TypeLits as HS
import qualified GHC.Records as HS
import qualified Data.Coerce as HS
import qualified Data.Bits as HS
import qualified Control.Monad as HS
import qualified RTS as RTS
import qualified Daedalus.RTS.Input as RTS
import qualified Daedalus.RTS.Map as Map
import qualified Daedalus.RTS.Vector as Vector
import qualified Daedalus.RTS.Numeric as RTS
import qualified Daedalus.RTS.JSON as RTS
import qualified Daedalus.RTS.Convert as RTSC
import qualified RTS.ParseError as RTS
import qualified RTS.Annot as RTS