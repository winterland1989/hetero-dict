{-# LANGUAGE CPP                    #-}
{-# LANGUAGE ConstraintKinds        #-}
{-# LANGUAGE DataKinds              #-}
{-# LANGUAGE FlexibleContexts       #-}
{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE GADTs                  #-}
{-# LANGUAGE KindSignatures         #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE ScopedTypeVariables    #-}
{-# LANGUAGE TypeFamilies           #-}
{-# LANGUAGE TypeOperators          #-}
{-# LANGUAGE UndecidableInstances   #-}
#if __GLASGOW_HASKELL__ < 710
{-# LANGUAGE OverlappingInstances   #-}
#endif

-- | Fast persistent heterogeneous list.
--
-- This module define 'DynDict', which wrap a 'KVList' linked-list,
-- benchmark showed that it's faster than previouse @Data.Sequence@ version,
-- since usually the element number is small(<20).
-- so even operations(add, get, modify, set)'s time complexity
-- are not as good as 'Seq', it's constantly faster in practice.
--
-- Typical usage: a heterogeneous state store, indexed by type level string.
--
-- @
-- > :set -XDataKinds -XQuasiQuotes
-- > let d =  add [key|foo|] 12 . add [key|bar|] "baz" $ empty
-- > get [key|foo|] d
-- 12
-- > get [key|bar|] d
-- "baz"
-- > let d' = set [key|foo|] 13 d
-- > get [key|foo|] d'
-- 13
-- @
--
module Data.Hetero.DynDict
    (
    -- ** DynDict
      DynDict
    , empty
    , add
    , InDict
    , get
    , modify
    , set
    , size
    -- ** re-export from KVList
    , key
    , KV(..)
    , KVList(..)
    , NotHasKey
    , Ix
    ) where

import           Data.Hetero.KVList
import           Data.Hetero.Dict (Store(..), ShowDict(..), mkDict)
import           Data.List          (intercalate)
import           GHC.TypeLits
import           Data.Proxy (Proxy(..))
import           Data.Aeson (ToJSON(..), FromJSON(..), Value(Object))
import qualified Data.HashMap.Strict as HM
import qualified Data.Text as T

--------------------------------------------------------------------------------

-- | heterogeneous persistent sequence.
--
newtype DynDict (kvs :: [KV *]) = DynDict (KVList kvs)

-- | A empty 'DynDict'.
--
empty :: DynDict '[]
empty = DynDict Empty
{-# INLINE empty #-}

-- | O(1) insert new k-v pair into 'DynDict'.
add :: (NotHasKey k kvs) => Proxy k -> v -> DynDict kvs -> DynDict (k ':= v ': kvs)
add _ v (DynDict kvs) = DynDict (Cons v kvs)
{-# INLINE add #-}

-- | Constraint ensure 'DynDict' must contain k-v pair.
--
class InDict (k :: Symbol) (v :: *) (kvs :: [KV *]) | k kvs -> v where
    get' :: Proxy k -> DynDict kvs -> v
    modify' ::  Proxy k -> (v -> v) -> DynDict kvs -> DynDict kvs

#if __GLASGOW_HASKELL__ >= 710
instance {-# OVERLAPPING #-} InDict k v (k ':= v ': kvs) where
#else
instance InDict k v (k ':= v ': kvs) where
#endif
    get' _ (DynDict (Cons v _)) =  v
    {-# INLINE get' #-}
    modify' _ f (DynDict (Cons v kvs)) = DynDict $ Cons (f v) kvs
    {-# INLINE modify' #-}

instance (InDict k v kvs, 'Index i ~ Ix k (k' ':= v' ': kvs), KnownNat i) => InDict k v (k' ':= v' ': kvs) where
    get' p (DynDict (Cons _ kvs)) =  get' p (DynDict kvs)
    {-# INLINE get' #-}
    modify' p f (DynDict (Cons v kvs)) =
        let DynDict kvs' = modify' p f (DynDict kvs)
        in DynDict (Cons v kvs')
    {-# INLINE modify' #-}

-- | O(m) get value using associated key.
--
get :: InDict k v kvs => Proxy k -> DynDict kvs -> v
get = get'

-- | O(m) modify value by associated key.
modify :: (InDict k v kvs) => Proxy k -> (v -> v) -> DynDict kvs -> DynDict kvs
modify = modify'

-- | O(m) modify value by associated key.
set :: (InDict k v kvs) => Proxy k -> v -> DynDict kvs -> DynDict kvs
set p v = modify' p (const v)
{-# INLINE set #-}

-- | O(n) size
size :: DynDict kvs -> Int
size (DynDict Empty) = 0
size (DynDict (Cons _ kvs)) = 1 + size (DynDict kvs)
{-# INLINE size #-}

--------------------------------------------------------------------------------

instance ShowDict kvs => Show (DynDict kvs) where
    show d@(DynDict kvs) = "DynDict {" ++
        (intercalate ", " . map (\(k, v, t) -> k ++ " = " ++ v ++ " :: " ++ show t) $ showDict 0 (mkDict s))
        ++ "}"
      where
        s = Store (size d) kvs

instance ToJSON (DynDict '[]) where
    toJSON _ = Object HM.empty

instance (KnownSymbol k, ToJSON v, ToJSON (DynDict kvs)) => ToJSON (DynDict (k ':= v ': kvs)) where
    toJSON (DynDict (Cons v kvs)) =
        let (Object obj) = toJSON (DynDict kvs)
            k = T.pack (symbolVal (Proxy :: Proxy k))
            obj' = HM.insert k (toJSON v) obj
        in Object obj'

instance FromJSON (DynDict '[]) where
    parseJSON (Object _) = return (DynDict Empty)
    parseJSON _          = fail "expect an object"

instance (KnownSymbol k, FromJSON v, FromJSON (DynDict kvs)) => FromJSON (DynDict (k ':= v ': kvs)) where
    parseJSON v@(Object obj) =
        let kString = symbolVal (Proxy :: Proxy k)
            k = T.pack kString
        in case HM.lookup k obj of
            Just v' -> do
                DynDict kvs <- parseJSON v
                v'' <- parseJSON v'
                return (DynDict (Cons v'' kvs))
            Nothing -> fail ("missing key: " ++ kString)
    parseJSON _          = fail "expect an object"
