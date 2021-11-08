{-# LANGUAGE FunctionalDependencies           #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE FlexibleContexts                 #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE DeriveGeneric                 #-}
{-# LANGUAGE DeriveAnyClass                 #-}
{-# LANGUAGE StandaloneDeriving                 #-}
{-# LANGUAGE PatternSynonyms                 #-}
{-# LANGUAGE TypeFamilies                 #-}
{-# LANGUAGE ConstraintKinds                 #-}

module Stack (
    Stack,
    stackPush_, stackPop_, stackIsEmpty_, emptyStack_,
    stackPush, stackPop, stackIsEmpty, emptyStack,
) where

import GHC.TypeLits
import GHC.Generics
import Data.Primitive
import qualified Data.Array.Accelerate as A

data Stack n a = Stack_ Int (A.Vec n a) deriving (Generic)
deriving instance (KnownNat n, A.VecElt a) => A.Elt (Stack n a)

type Stacking n a = (KnownNat n, A.VecElt a, A.Vectoring (A.Exp (A.Vec n a)) (A.Exp a))

pattern Stack :: (KnownNat n, A.VecElt a) => A.Exp Int -> A.Exp (A.Vec n a) -> A.Exp (Stack n a)
pattern Stack headIdx dat = A.Pattern (headIdx, dat)

stackPush_ :: (A.Vectoring (A.Vec n a) a) => Stack n a -> a -> Stack n a
stackPush_ (Stack_ headIdx dat) v = Stack_ (headIdx+1) (A.vecWrite dat headIdx v)

stackPop_ :: (A.Vectoring (A.Vec n a) a) => Stack n a -> (Stack n a, a)
stackPop_ (Stack_ headIdx dat) = (Stack_ (headIdx-1) dat, A.vecIndex dat (headIdx-1))

stackIsEmpty_ :: Stack n a -> Bool
stackIsEmpty_ (Stack_ 0 _) = True
stackIsEmpty_ _      = False

emptyStack_ :: (KnownNat n, Prim a) => Stack n a
emptyStack_ = Stack_ 0 A.vecEmpty

stackPush :: (Stacking n a) => A.Exp (Stack n a) -> A.Exp a -> A.Exp (Stack n a)
stackPush (Stack headIdx dat) v = Stack (headIdx+1) (A.vecWrite dat headIdx v)

stackPop :: (Stacking n a) => A.Exp (Stack n a) -> A.Exp (Stack n a, a)
stackPop (Stack headIdx dat) = A.T2 (Stack (headIdx-1) dat) (A.vecIndex dat (headIdx-1))

stackIsEmpty :: (Stacking n a) => A.Exp (Stack n a) -> A.Exp Bool
stackIsEmpty (Stack headIdx _) = headIdx A.== 0 A.? (A.True_, A.False_)

emptyStack :: (Stacking n a) => A.Exp (Stack n a)
emptyStack = Stack 0 A.vecEmpty
