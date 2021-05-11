{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilyDependencies #-}
{-# LANGUAGE UndecidableInstances #-}

module Structures (
    Nat(..), NatTerm(..), KnownNat(..),
    Stack,
    emptyStack, emptyStack',
    stackMap, stackMap',
    stackPush, stackPush',
    stackPop, stackPop',
    stackIsEmpty, stackIsEmpty',
    stackLength, stackLength'
) where

import qualified Data.Array.Accelerate as A
import qualified Data.Array.Accelerate.Unsafe as A

-- Internal Accelerate things
import qualified Data.Array.Accelerate.Sugar.Elt as AI
import qualified Data.Array.Accelerate.Representation.Tag as AI
import qualified Data.Array.Accelerate.Representation.Type as AI

import Data.Proxy

data Nat = NZ | NS Nat
  deriving (Show)

data NatTerm n where
    ZZ :: NatTerm 'NZ
    SS :: NatTerm n -> NatTerm ('NS n)

class KnownNat (n :: Nat) where inferNat :: NatTerm n
instance KnownNat 'NZ where inferNat = ZZ
instance KnownNat n => KnownNat ('NS n) where inferNat = SS inferNat

-- Evidence that a type is an instance of a particular class; or,
-- operationally: a reified type class dictionary
data Has c (a :: k) where
    Has :: c a => Has c a

natTermIsKnown :: NatTerm n -> Has KnownNat n
natTermIsKnown ZZ = Has
natTermIsKnown (SS nn) | Has <- natTermIsKnown nn = Has

pileHasElt :: A.Elt a => Proxy a -> NatTerm n -> Has A.Elt (Pile n a)
pileHasElt _ ZZ = Has
pileHasElt p (SS nn) | Has <- pileHasElt p nn = Has

type family Pile (n :: Nat) a = r | r -> n where
    Pile 'NZ a = ()
    Pile ('NS n) a = (Pile n a, a)


-- Stack bottom is at root of the tuple list; stack tip is n down towards the ()
data Stack (n :: Nat) a = Stack_ Int (Pile n a)

-- Custom 'Elt' instance because I don't want to figure out how to do it using
-- the automatic stuff; 'Pile' messes things up
instance (KnownNat n, A.Elt a, A.Elt (Pile n a)) => A.Elt (Stack n a) where
    type EltR (Stack n a) = (((), AI.EltR Int), AI.EltR (Pile n a))
    eltR = AI.TupRpair (AI.TupRpair AI.TupRunit (AI.eltR @Int)) (AI.eltR @(Pile n a))
    tagsR = do
        inttag <- AI.tagsR @Int
        piletag <- AI.tagsR @(Pile n a)
        return $ AI.TagRpair (AI.TagRpair AI.TagRunit inttag) piletag
    fromElt (Stack_ len pr) = (((), AI.fromElt len), AI.fromElt pr)
    toElt (((), len'), pr') = Stack_ (AI.toElt len') (AI.toElt pr')

pattern Stack :: (KnownNat n, A.Elt a, A.Elt (Pile n a))
              => A.Exp Int -> A.Exp (Pile n a) -> A.Exp (Stack n a)
pattern Stack len pr = A.Pattern (len, pr)
{-# COMPLETE Stack #-}

emptyPile :: forall n a. A.Elt a => Proxy a -> NatTerm n -> A.Exp (Pile n a)
emptyPile _ ZZ = A.constant ()
emptyPile proxy (SS nn) =
    let p = emptyPile proxy nn
    in case pileHasElt (Proxy :: Proxy a) nn of
         Has -> A.T2 p A.undef

emptyStack' :: forall n a. A.Elt a => NatTerm n -> A.Exp (Stack n a)
emptyStack' nn
  | Has <- pileHasElt (Proxy :: Proxy a) nn
  , Has <- natTermIsKnown nn
  = Stack 0 (emptyPile (Proxy :: Proxy a) nn)

emptyStack :: forall n a. (A.Elt a, KnownNat n) => A.Exp (Stack n a)
emptyStack = emptyStack' inferNat

pileMap :: forall n a b. (A.Elt a, A.Elt b)
        => NatTerm n -> (A.Exp a -> A.Exp b) -> A.Exp (Pile n a) -> A.Exp (Pile n b)
pileMap ZZ _ _ = A.constant ()
pileMap (SS nn) f prep
  | Has <- pileHasElt (Proxy @a) nn
  , Has <- pileHasElt (Proxy @b) nn
  , A.T2 p x <- prep  -- needs evidence from 'pileHasElt'
  = A.T2 (pileMap nn f p) (f x)

stackMap' :: forall n a b. (A.Elt a, A.Elt b)
         => NatTerm n -> (A.Exp a -> A.Exp b) -> A.Exp (Stack n a) -> A.Exp (Stack n b)
stackMap' nn topf stackexpr
  | Has <- natTermIsKnown nn
  , Has <- pileHasElt (Proxy @a) nn
  , Has <- pileHasElt (Proxy @b) nn
  , Stack len p <- stackexpr  -- needs evidence from 'pileHasElt'/'natTermIsKnown'
  = Stack len (pileMap inferNat topf p)

stackMap :: forall n a b. (A.Elt a, A.Elt b, KnownNat n)
         => (A.Exp a -> A.Exp b) -> A.Exp (Stack n a) -> A.Exp (Stack n b)
stackMap = stackMap' inferNat

-- This becomes a right-leaning tree of conditionals
pilePush :: forall n a. A.Elt a => NatTerm n -> Int -> A.Exp Int -> A.Exp (Pile n a) -> A.Exp a -> A.Exp (Int, Pile n a)
pilePush ZZ _ len _ _ = A.undef 
pilePush (SS nn) depth len prep x
  | Has <- pileHasElt (Proxy @a) nn
  , A.T2 p y <- prep  -- needs evidence from 'pileHasElt'
  = A.cond (len A.== A.constant depth)
           (A.T2 (len + 1) (A.T2 p x))
           (case pilePush nn (depth + 1) len p x of
              A.T2 len' p' -> A.T2 len' (A.T2 p' y))

pilePop :: forall n a. A.Elt a => NatTerm n -> Int -> A.Exp Int -> A.Exp (Pile n a) -> A.Exp a
pilePop ZZ _ len _ = A.undef
pilePop (SS nn) depth len prep
    | Has <- pileHasElt (Proxy @a) nn
    , A.T2 p y <- prep
    = A.cond (len A.== A.constant depth)
             y
             (pilePop nn (depth+1) len p)
        

stackPush' :: forall n a. A.Elt a => NatTerm n -> A.Exp (Stack n a) -> A.Exp a -> A.Exp (Stack n a)
stackPush' nn stackexpr x
  | Has <- natTermIsKnown nn
  , Has <- pileHasElt (Proxy @a) nn
  , Stack len p <- stackexpr  -- needs evidence from 'pileHasElt'/'natTermIsKnown'
  , A.T2 len' p' <- pilePush nn 0 len p x
  = Stack (len'+1) p'

stackPush :: forall n a. (KnownNat n, A.Elt a) => A.Exp (Stack n a) -> A.Exp a -> A.Exp (Stack n a)
stackPush = stackPush' inferNat

stackPop' :: forall n a. (A.Elt a) => NatTerm n -> A.Exp (Stack n a) -> A.Exp (a, Stack n a)
stackPop' nn stackexpr
    | Has <- natTermIsKnown nn
    , Has <- pileHasElt (Proxy @a) nn
    , Stack len p <- stackexpr
    , y <- pilePop @n @a nn 0 0 p
    = A.T2 y (Stack (len-1) p)

stackPop :: forall n a. (A.Elt a, KnownNat n) => A.Exp (Stack n a) -> A.Exp (a, Stack n a)
stackPop = stackPop' inferNat

stackIsEmpty' :: forall n a. (A.Elt a) => NatTerm n -> A.Exp (Stack n a) -> A.Exp Bool
stackIsEmpty' nn stackexpr
    | Has <- natTermIsKnown nn
    , Has <- pileHasElt (Proxy @a) nn
    , Stack len p <- stackexpr
    = len A.== 0

stackIsEmpty :: forall n a. (KnownNat n, A.Elt a) => A.Exp (Stack n a) -> A.Exp Bool
stackIsEmpty = stackIsEmpty' inferNat

stackLength :: forall n a. (A.Elt a, KnownNat n) => A.Exp (Stack n a) -> A.Exp Int
stackLength = stackLength' inferNat

stackLength' :: forall n a. (A.Elt a) => NatTerm n -> A.Exp (Stack n a) -> A.Exp Int
stackLength' nn stackexpr
    | Has <- natTermIsKnown nn
    , Has <- pileHasElt (Proxy @a) nn
    , Stack len _ <- stackexpr
    = len

