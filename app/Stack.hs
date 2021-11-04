{-# LANGUAGE FunctionalDependencies           #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE FlexibleContexts                 #-}
{-# LANGUAGE GADTs                 #-}

module Stack (
    Stack,
    stackPush_, stackPop_, stackIsEmpty_, emptyStack_,
    stackPush, stackPop, stackIsEmpty, emptyStack,
) where

import GHC.TypeLits
import Data.Primitive
import qualified Data.Array.Accelerate as A

type Stack n a = (Int, A.Vec n a)

stackPush_ :: (A.Vectoring (A.Vec n a) a) => Stack n a -> a -> Stack n a
stackPush_ (headIdx, dat) v = (headIdx+1, A.vecWrite dat headIdx v)

stackPop_ :: (A.Vectoring (A.Vec n a) a) => Stack n a -> (Stack n a, a)
stackPop_ (headIdx, dat) = ((headIdx-1, dat), A.vecIndex dat (headIdx-1))

stackIsEmpty_ :: Stack n a -> Bool
stackIsEmpty_ (0, _) = True
stackIsEmpty_ _      = False

emptyStack_ :: (KnownNat n, Prim a) => Stack n a
emptyStack_ = (0, A.vecEmpty)

stackPush :: (KnownNat n, A.VecElt a, A.Vectoring (A.Exp (A.Vec n a)) (A.Exp a)) => A.Exp (Stack n a) -> A.Exp a -> A.Exp (Stack n a)
stackPush (A.T2 headIdx dat) v = A.T2 (headIdx+1) (A.vecWrite dat headIdx v)

stackPop :: (KnownNat n, A.VecElt a, A.Vectoring (A.Exp (A.Vec n a)) (A.Exp a)) => A.Exp (Stack n a) -> A.Exp (Stack n a, a)
stackPop (A.T2 headIdx dat) = A.T2 (A.T2 (headIdx-1) dat) (A.vecIndex dat (headIdx-1))

stackIsEmpty :: (KnownNat n, A.VecElt a) => A.Exp (Stack n a) -> A.Exp Bool
stackIsEmpty (A.T2 headIdx _) = headIdx A.== 0 A.? (A.True_, A.False_)

emptyStack :: (A.VecElt a, KnownNat n) => A.Exp (Stack n a)
emptyStack = A.T2 0 A.vecEmpty
