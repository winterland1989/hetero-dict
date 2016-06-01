{-# LANGUAGE BangPatterns           #-}
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
{-# LANGUAGE TypeOperators          #-}
{-# LANGUAGE UndecidableInstances   #-}
#if __GLASGOW_HASKELL__ < 710
{-# LANGUAGE OverlappingInstances   #-}
#endif

-- | Fast read-only heterogeneous array.
--
--  This module is extracted from <http://hackage.haskell.org/package/web-routing web-routing>,
--  orginally desgined for high performance type safe routing.
--  The basic idea is:
--
--      1. Construct a heterogeneous linked-list is O(n), since prepend is O(1).
--
--      2. Convert it into a heterogeneous array in O(n).
--
--      3. Following access will be a simple O(1) array indexing,
--      with index computed at compile time so you can't get missing keys.
--
-- Typical usage: a heterogeneous lookup table, indexed by type level string.
--
-- @
-- > :set -XDataKinds -XQuasiQuotes
-- > let d = mkDict . add [key|foo|] 12 . add [key|bar|] "baz" $ emptyStore
-- > get [key|foo|] d
-- 12
-- > get [key|bar|] d
-- "baz"
-- @
--
module Data.Hetero.Dict
    (
    -- ** Store
      Store
    , emptyStore
    , add
    -- ** Dict
    , Dict
    , mkDict'
    , mkDict
    , InDict
    , get
    -- ** re-export from KVList
    , key
    , KV(..)
    , KVList(..)
    , NotHasKey
    , Ix
    -- ** helpers
    , ShowDict(..)
    ) where

import qualified Control.Monad.Primitive as P
import           Control.Monad.ST        (ST, runST)
import           Data.Hetero.KVList
import           Data.List               (intercalate)
import qualified Data.Primitive.Array    as P
import           Data.Proxy
import           Data.Typeable           (TypeRep, Typeable, typeOf)
import           GHC.Exts                (Any)
import           GHC.TypeLits
import           Unsafe.Coerce

--------------------------------------------------------------------------------

-- | Heterogeneous linked-list with a size field.
--
-- use 'mkDict' to convert it into a 'Dict'.
--
data Store kvs = Store
    { storeSize :: {-# UNPACK #-} !Int
    , storeBody :: KVList kvs
    }

-- | An empty 'Store'
--
emptyStore :: Store '[]
emptyStore = Store 0 Empty
{-# INLINABLE emptyStore #-}


-- | O(1) add key value pair to 'Store'.
--
-- @
-- > let a = add [key|foo|] (12 :: Int) emptyStore
-- > a
-- Store {foo = 12 :: Int}
-- > add [key|bar|] "baz" a
-- Store {bar = "baz" :: [Char], foo = 12 :: Int}
-- @
--
add :: (NotHasKey k kvs) => proxy k -> v -> Store kvs -> Store (k ':= v ': kvs)
add _ v (Store l c) = Store (l + 1) (Cons v c)
{-# INLINABLE add #-}

--------------------------------------------------------------------------------

-- | Read-Only heterogeneous array.
--
-- The underline data structure is a boxed array,
-- support 'get' operation only.
newtype Dict (kvs :: [KV *]) = Dict (P.Array Any)

-- | O(n) convert a 'Store' into a 'Dict' inside 'ST' monad.
--
mkDict' :: forall s kvs. Store kvs -> ST s (Dict kvs)
mkDict' store = do
    ary <- P.newArray (storeSize store) undefined
    go ary
    Dict `fmap` P.unsafeFreezeArray ary
  where
    go :: P.MutableArray (P.PrimState (ST s)) Any -> ST s ()
    go array = loop 0 (storeBody store)
      where
        loop :: Int -> KVList kvs' -> ST s ()
        loop !i (Cons v ss) = do
            P.writeArray array i (unsafeCoerce v)
            loop (i + 1) ss
        loop _ Empty = return ()
{-# INLINABLE mkDict' #-}

-- | O(n) convert "Store" to 'Dict'.
mkDict :: Store kvs -> Dict kvs
mkDict store = runST $ mkDict' store
{-# INLINABLE mkDict #-}

getImpl :: forall i proxy k kvs v. ('Index i ~ Ix k kvs, KnownNat i) => proxy (k :: Symbol) -> Dict kvs -> v
getImpl _ (Dict d) = unsafeCoerce $ d `P.indexArray` fromIntegral (natVal (Proxy :: Proxy i))
{-# INLINABLE getImpl #-}

-- | Constraint ensure 'Dict' must contain k-v pair.
--
class InDict (k :: Symbol) (v :: *) (kvs :: [KV *]) | k kvs -> v where
    get' :: proxy k -> Dict kvs -> v

#if __GLASGOW_HASKELL__ >= 710
instance {-# OVERLAPPING #-} InDict k v (k ':= v ': kvs) where
#else
instance InDict k v (k ':= v ': kvs) where
#endif
    get' = getImpl
    {-# INLINE get' #-}

instance (InDict k v kvs, 'Index i ~ Ix k (k' ':= v' ': kvs), KnownNat i) => InDict k v (k' ':= v' ': kvs) where
    get' = getImpl
    {-# INLINE get' #-}

-- | O(1) get value using associated key from 'Dict'.
--
get :: InDict k v kvs => proxy k -> Dict kvs -> v
get = get'
{-# INLINE get #-}

--------------------------------------------------------------------------------

-- | Helper class for defining store's 'Show' instance.
class ShowDict (kvs :: [KV *]) where
    showDict :: Int -> Dict kvs -> [(String, String, TypeRep)]

instance ShowDict '[] where
    showDict _ _ = []

instance (KnownSymbol k, Typeable v, Show v, ShowDict kvs) => ShowDict (k ':= v ': kvs) where
    showDict i (Dict t) =
        (symbolVal (Proxy :: Proxy k), show (unsafeCoerce $ P.indexArray t i :: v), typeOf (undefined :: v)):
        showDict (i + 1) (unsafeCoerce $ Dict t :: Dict kvs)

instance ShowDict kvs => Show (Dict kvs) where
    show d = "Dict {" ++
        (intercalate ", " . map (\(k, v, t) -> k ++ " = " ++ v ++ " :: " ++ show t) $ showDict 0 d)
        ++ "}"

instance ShowDict kvs => Show (Store kvs) where
    show d = "Store {" ++
        (intercalate ", " . map (\(k, v, t) -> k ++ " = " ++ v ++ " :: " ++ show t) $ showDict 0 (mkDict d))
        ++ "}"
