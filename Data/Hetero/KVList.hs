{-# LANGUAGE ConstraintKinds        #-}
{-# LANGUAGE DataKinds              #-}
{-# LANGUAGE FlexibleContexts       #-}
{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE GADTs                  #-}
{-# LANGUAGE KindSignatures         #-}
{-# LANGUAGE ScopedTypeVariables    #-}
{-# LANGUAGE TemplateHaskell        #-}
{-# LANGUAGE TypeFamilies           #-}
{-# LANGUAGE TypeOperators          #-}
{-# LANGUAGE UndecidableInstances   #-}

-- | A simple heterogeneous linked-list for k-v pairs.
--
module Data.Hetero.KVList
    ( KV(..)
    , KVList(..)
    , key
    , AddKey
    , AddResult(..)
    , NotHasKey
    , GetResult(..)
    , Ix
    ) where

import           Data.Proxy
import           GHC.TypeLits
import qualified Language.Haskell.TH       as TH
import           Language.Haskell.TH.Quote (QuasiQuoter (..))

--------------------------------------------------------------------------------

-- | (kind) key-value pair
--
data KV v = Symbol := v

-- | A simple heterogeneous kv linked-list.
--
data KVList (kvs :: [KV *]) where
    Cons  :: v -> KVList kvs -> KVList (k ':= v ': kvs)
    Empty :: KVList '[]


-- | Quoter for constructing string literal proxy.
--
-- @[key|foo|] == (Proxy :: Proxy "foo")@
--
key :: QuasiQuoter
key = QuasiQuoter
    { quoteExp  = \s -> [| Proxy :: Proxy $(TH.litT $ TH.strTyLit s) |]
    , quotePat  = error "key qq only exp or type."
    , quoteType = \s -> [t| Proxy $(TH.litT $ TH.strTyLit s) |]
    , quoteDec  = error "key qq only exp or type."
    }

-- | (kind) pretty print type error for 'NotHasKey'.
--
-- @
-- > add [key|foo|] 12 $ add [key|foo|] "a" emptyStore
-- Couldn't match type ‘'DuplicatedKey "foo"’ with ‘'HasKey "foo"’
-- @
--
data AddResult = HasKey Symbol | DuplicatedKey Symbol

-- | Add a key's type to 'KVList' if not existed.
--
type family AddKey (k :: Symbol) (kvs :: [KV *]) :: AddResult where
    AddKey k '[] = 'HasKey k
    AddKey k (k  ':= v ': kvs) = 'DuplicatedKey k
    AddKey k (k' ':= v ': kvs) = AddKey k kvs

-- | Constraint ensure a key will be inserted into 'Store'.
--
type NotHasKey k kvs = AddKey k kvs ~ 'HasKey k

-- | (kind) pretty print type error for 'Ix'
--
-- @
-- > get [key|b|] (mkDict $ add [key|a|] 123 emptyStore)
-- Couldn't match type ‘'Index i0’ with ‘'NotFoundKey "b"’
-- @
--
data GetResult = Index Nat | NotFoundKey Symbol

type family Ix' (i :: Nat) (k :: Symbol) (kvs :: [KV *]) :: GetResult where
  Ix' i k '[] = 'NotFoundKey k
  Ix' i k (k  ':= v ': kvs) = 'Index i
  Ix' i k (k' ':= v ': kvs) = Ix' (i + 1) k kvs


-- | Indexing a key at compile time.
--
type Ix k kvs = Ix' 0 k kvs
