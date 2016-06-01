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

-- | Fast persistent heterogeneous sequence.
--
-- This module define 'DynDict', which use 'S.Seq' as underline data structure,
-- so all operations(add, get, modify, set)'s time complexity are similar.
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
    -- ** re-export from KVList
    , key
    , KV(..)
    , KVList(..)
    , NotHasKey
    , Ix
    -- ** Internal helpers
    , ShowDynDict(..)
    ) where

import           Data.Hetero.KVList
import           Data.List          (intercalate)
import           Data.Proxy
import qualified Data.Sequence      as S
import           Data.Typeable      (TypeRep, Typeable, typeOf)
import           GHC.Exts           (Any)
import           GHC.TypeLits
import           Unsafe.Coerce

--------------------------------------------------------------------------------

-- | heterogeneous persistent sequence.
--
-- The underline data structure is 'S.Seq'.
-- support efficient 'add', 'get' and 'modify' operations.
newtype DynDict (kvs :: [KV *]) = DynDict (S.Seq Any)


-- | A empty 'DynDict'.
--
empty :: DynDict '[]
empty = DynDict S.empty
{-# INLINABLE empty #-}

-- | O(1) insert new k-v pair into 'DynDict'.
add :: (NotHasKey k kvs) => proxy k -> v -> DynDict kvs -> DynDict (k ':= v ': kvs)
add _ v (DynDict d) = DynDict (unsafeCoerce v S.<| d)
{-# INLINE add #-}

getImpl :: forall i proxy k kvs v. ('Index i ~ Ix k kvs, KnownNat i) => proxy (k :: Symbol) -> DynDict kvs -> v
getImpl _ (DynDict d) = unsafeCoerce $ d `S.index` fromIntegral (natVal (Proxy :: Proxy i))
{-# INLINABLE getImpl #-}

modifyImpl :: forall i proxy k kvs v. ('Index i ~ Ix k kvs, KnownNat i) => proxy (k :: Symbol) -> (v -> v) -> DynDict kvs -> DynDict kvs
modifyImpl _ f (DynDict d) = DynDict $
    S.adjust (unsafeCoerce . f . unsafeCoerce) (fromIntegral (natVal (Proxy :: Proxy i))) d
{-# INLINABLE modifyImpl #-}

-- | Constraint ensure 'DynDict' must contain k-v pair.
--
class InDict (k :: Symbol) (v :: *) (kvs :: [KV *]) | k kvs -> v where
    get' :: proxy k -> DynDict kvs -> v
    modify' ::  proxy k -> (v -> v) -> DynDict kvs -> DynDict kvs

#if __GLASGOW_HASKELL__ >= 710
instance {-# OVERLAPPING #-} InDict k v (k ':= v ': kvs) where
#else
instance InDict k v (k ':= v ': kvs) where
#endif
    get' = getImpl
    {-# INLINE get' #-}
    modify' = modifyImpl
    {-# INLINE modify' #-}

instance (InDict k v kvs, 'Index i ~ Ix k (k' ':= v' ': kvs), KnownNat i) => InDict k v (k' ':= v' ': kvs) where
    get' = getImpl
    {-# INLINE get' #-}
    modify' = modifyImpl
    {-# INLINE modify' #-}

-- | O(log(min(i,n-i))) get value using associated key.
--
get :: InDict k v kvs => proxy k -> DynDict kvs -> v
get = get'
{-# INLINE get #-}

-- | O(log(min(i,n-i))) modify value by associated key.
modify :: (InDict k v kvs) => proxy k -> (v -> v) -> DynDict kvs -> DynDict kvs
modify = modify'
{-# INLINE modify #-}

-- | O(log(min(i,n-i))) modify value by associated key.
set :: (InDict k v kvs) => proxy k -> v -> DynDict kvs -> DynDict kvs
set p v = modify' p (const v)
{-# INLINE set #-}

--------------------------------------------------------------------------------

-- | Helper class for defining store's 'Show' instance.
class ShowDynDict (kvs :: [KV *]) where
    showDict :: Int -> DynDict kvs -> [(String, String, TypeRep)]

instance ShowDynDict '[] where
    showDict _ _ = []

instance (KnownSymbol k, Typeable v, Show v, ShowDynDict kvs) => ShowDynDict (k ':= v ': kvs) where
    showDict i (DynDict t) =
        (symbolVal (Proxy :: Proxy k), show (unsafeCoerce $ t `S.index` i :: v), typeOf (undefined :: v)):
        showDict (i + 1) (unsafeCoerce $ DynDict t :: DynDict kvs)

instance ShowDynDict kvs => Show (DynDict kvs) where
    show d = "DynDict {" ++
        (intercalate ", " . map (\(k, v, t) -> k ++ " = " ++ v ++ " :: " ++ show t) $ showDict 0 d)
        ++ "}"
