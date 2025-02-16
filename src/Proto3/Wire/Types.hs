{-# LANGUAGE DeriveDataTypeable         #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE DeriveLift                 #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

{-
  Copyright 2016 Awake Networks

  Licensed under the Apache License, Version 2.0 (the "License");
  you may not use this file except in compliance with the License.
  You may obtain a copy of the License at

      http://www.apache.org/licenses/LICENSE-2.0

  Unless required by applicable law or agreed to in writing, software
  distributed under the License is distributed on an "AS IS" BASIS,
  WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
  See the License for the specific language governing permissions and
  limitations under the License.
-}

-- | This module defines types which are shared by the encoding and decoding
-- modules.

module Proto3.Wire.Types
    ( -- * Message Structure
      FieldNumber(..)
    , fieldNumber
    , WireType(..)
    ) where

import           Control.DeepSeq            ( NFData )
import           Data.Data                  ( Data )
-- import           Data.Hashable              ( Hashable )
import           Data.Word                  ( Word64 )
-- import           GHC.Generics               ( Generic )
-- import           Language.Haskell.TH.Syntax ( Lift )

-- | A 'FieldNumber' identifies a field inside a protobufs message.
--
-- This library makes no attempt to generate these automatically, or even make
-- sure that field numbers are provided in increasing order. Such things are
-- left to other, higher-level libraries.
newtype FieldNumber = FieldNumber
  { getFieldNumber :: Word64 }
  deriving (Bounded, Data, Enum, Eq, NFData, Num, Ord)

instance Show FieldNumber where
  show (FieldNumber n) = show n

-- | Create a 'FieldNumber' given the (one-based) integer which would label
-- the field in the corresponding .proto file.
fieldNumber :: Word64 -> FieldNumber
fieldNumber = FieldNumber

-- | The (non-deprecated) wire types identified by the Protocol
-- Buffers specification.
data WireType
  = Varint
  | Fixed32
  | Fixed64
  | LengthDelimited
  deriving (Bounded, Data, Enum, Eq, Ord, Show)
