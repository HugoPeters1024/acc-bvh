{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE ImplicitParams #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE MagicHash #-}
module Main where


import Prelude as P
import LinAlg
import qualified Graphics.Gloss as Gloss
import Data.Array.Accelerate.LLVM.PTX as PTX
import Data.Array.Accelerate.Interpreter as Interpreter
import Data.Array.Accelerate as A
import qualified Graphics.Gloss.Accelerate.Raster.Array as G
import Data.Array.Accelerate.Linear.V3 as V3
import Data.Array.Accelerate.Linear.Metric
import Data.Array.Accelerate.Linear.Matrix
import Control.Monad.Trans.Reader
import Control.Monad.Identity
import GHC.Generics
import GHC.TypeNats

import Structures

type Scene = Acc (Vector Triangle, Vector BVH)

data BVH = BVH_ Bool Int Int
    deriving (Generic, Elt)

pattern BVH :: Exp Bool -> Exp Int -> Exp Int -> Exp BVH
pattern BVH leaf l r = A.Pattern (leaf, l, r)

pattern Leaf :: Exp Int -> Exp Int -> Exp BVH
pattern Leaf l r = BVH False_ l r

pattern Node :: Exp Int -> Exp Int -> Exp BVH
pattern Node l r = BVH True_ l r


isLeaf :: Exp BVH -> Exp Bool
isLeaf (Leaf _ _) = True_
isLeaf (Node _ _) = False_

lol :: [Exp BVH]
lol = [Leaf 0 0, Node 4 4]

traverseBVH :: BVH -> Exp Ray -> Exp TriangleHitInfo
traverseBVH bvh ray = let
    stack :: Acc (Matrix BVH)
    stack = undefined

    emtpyHit = TriangleHitInfo 0 0 0 Nothing_
    in undefined

vecToColor :: Exp V3f -> Exp G.Colour
vecToColor (V3_ x y z) = G.rgba x y z 1

hitInfoToColor :: Exp TriangleHitInfo -> Exp G.Colour
hitInfoToColor (TriangleHitInfo t u v ti) = ti & match \case 
    Nothing_ -> G.rgba 0 0 0 0
    Just_ _  -> G.rgba u v 0 0

transform :: Exp Float -> Exp Mat4f
transform time = identity `mtranslate` V3_ (time/10) 0 0

triangle :: Exp DIM1 -> Exp Triangle
triangle (I1 i) = Triangle 
            (V3_ 0   0 1)
            (V3_ 1   0 1)
            (V3_ 0.5 1 1)

mkScene :: Exp Float -> Acc (Vector Triangle, Vector BVH)
mkScene time = T2 (A.generate (I1 1) triangle) (A.generate (I1 1) (const (Leaf 0 0)))


genRay :: Exp Float -> Exp Float -> Exp Ray
genRay x y = 
    let
        target = V3_ x y 0
        origin = V3_ 0.5 0.5 (-1)
        direction = normalize (target - origin)
    in Ray origin direction

pnext1 :: Exp DIM1 -> Exp DIM1
pnext1 (I1 i) = I1 (i+1)

type ShortStack a = Stack ('NS 'NZ) a

type WState = Acc (Vector TriangleHitInfo, Scalar (ShortStack BVH))

liftAcc :: (Elt a, Elt b) => (Exp a -> Exp b) -> (Acc (Scalar a) -> Acc (Scalar b))
liftAcc f = unit . f . the

sceneIntersect :: Scene -> Acc (Vector Ray) -> Acc (Vector TriangleHitInfo)
sceneIntersect (T2 triangles bvh) rays =
    let
        startStack :: Exp (ShortStack BVH)
        startStack = stackPush emptyStack (bvh!I1 0)

        initialWhileState :: WState
        initialWhileState = lift (A.map (const (TriangleHitInfo 0 0 0 Nothing_)) rays, unit startStack)

        whileCond :: WState -> Acc (Scalar Bool)
        whileCond (T2 _ c) = unit False_ --liftAcc (A.not . stackIsEmpty) c 

        itWhile :: WState -> WState
        itWhile (T2 hs s) = T2 hs (A.map (A.snd . stackPop) s)

        in A.afst $ A.awhile whileCond itWhile initialWhileState


rayIntersect :: Exp DIM1 -> Exp Triangle -> Exp Ray -> Exp TriangleHitInfo
rayIntersect i triangle ray = rayTriangleIsect triangle ray & match \case
    Nothing_ -> TriangleHitInfo 0 0 0 Nothing_
    Just_ (T3 t u v) -> TriangleHitInfo t u v (Just_ i)

initialRays :: Acc (Vector Ray)
initialRays = A.flatten $ A.generate (I2 640 480) (\(I2 y x) -> genRay (divF x 640) (divF y 480))

white :: Exp G.Colour
white = G.rgba 1 1 1 1

divF :: Exp Int -> Exp Int -> Exp Float
divF a b = A.fromIntegral a / A.fromIntegral b

render :: Acc (Array DIM2 G.Colour)
render = let
        rays = initialRays
        scene = mkScene 0
        pixels = A.map hitInfoToColor (sceneIntersect scene rays)
     in reshape (I2 640 480) pixels

main :: IO ()
main = G.playArrayWith PTX.run1
    (G.InWindow "BVH" (640, 480) (10,10))
    (1,1)
    60
    ()
    (const ())
    (const render)
    (const id)
    (const id)
