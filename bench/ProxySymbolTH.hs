{-# LANGUAGE TemplateHaskell, QuasiQuotes #-}
{-# LANGUAGE RecordWildCards, NamedFieldPuns #-}

{-# LANGUAGE DataKinds, ScopedTypeVariables, ExplicitNamespaces, TypeOperators, FlexibleContexts, PolyKinds #-}

module ProxySymbolTH where -- TODO i'll move this out somewhere

import Data.Vinyl
-- import Data.Vinyl.Notation (type (∈))

import Language.Haskell.TH
import Language.Haskell.TH.Quote
import Data.Proxy
import GHC.TypeLits

--------------------------------------------------------------------------------

-- | get a value from its key in the record
fget
 :: forall s a fields proxy. ('(s,a) ∈ fields)
 => proxy s -> FieldRec fields -> a
fget _ record = getField $ rget (Proxy :: Proxy '(s,a)) record

fmodify
  :: forall s a fields proxy. ('(s,a) ∈ fields)
  => proxy s -> (a -> a) -> FieldRec fields -> FieldRec fields
fmodify _ function input = output
 where
 output = rput b input
 b = fieldMap function a
 a = rget (Proxy :: Proxy '(s,a)) input

-- | construct a key-value pair
field :: (KnownSymbol s) => proxy s -> a -> ElField '(s,a)
field _ a = Field a

--------------------------------------------------------------------------------

-- | splices a proxy for a type-level string
ps :: QuasiQuoter
ps = defaultQuasiQuoter{quoteExp}
 where
 quoteExp = proxySymbolQ

proxySymbolQ :: String -> ExpQ
proxySymbolQ = proxyExpQ . litT . strTyLit

proxyTypeQ :: TypeQ -> TypeQ
proxyTypeQ t = appT (conT proxy_tc) t

proxyExpQ :: TypeQ -> ExpQ
proxyExpQ t = sigE (conE proxy_d) (proxyTypeQ t)

-- makeSymbol :: String -> Type
-- makeSymbol = LitT . StrTyLit

proxy_tc :: Name
proxy_tc = ''Proxy

proxy_d :: Name
proxy_d = 'Proxy

defaultQuasiQuoter :: QuasiQuoter
defaultQuasiQuoter = QuasiQuoter{..}
 where

 quoteExp  :: String -> Q Exp
 quoteExp  = error $ message "expressions"

 quotePat  :: String -> Q Pat
 quotePat  = error $ message "patterns"

 quoteType :: String -> Q Type
 quoteType = error $ message "types"

 quoteDec  :: String -> Q [Dec]
 quoteDec  = error $ message "declarations"

 message x = "this QuasiQuoter doesn't support " ++ x
