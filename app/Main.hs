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
{-# LANGUAGE TypeApplications #-}
module Main where


import Debug.Trace
import Prelude as P
import LinAlg
import qualified Graphics.Gloss as Gloss
import Data.Array.Accelerate.LLVM.PTX as PTX
import Data.Array.Accelerate.Interpreter as Interpreter
import Data.Array.Accelerate as A
import Data.Array.Accelerate.Error as A
import Data.Array.Accelerate.Debug as A
import qualified Data.Array.Accelerate.Unsafe as A
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

bbox :: BB
bbox = BB_ (V3.V3 (0) (0) (-1)) (V3.V3 100 100 100)

data BVH = BVH_ Bool Int Int BB
    deriving (Generic, Elt, Show)

pattern BVH :: Exp Bool -> Exp Int -> Exp Int -> Exp BB -> Exp BVH
pattern BVH leaf l r bb = A.Pattern (leaf, l, r, bb)

pattern Leaf :: Exp Int -> Exp Int -> Exp BB -> Exp BVH
pattern Leaf l r bb = BVH False_ l r bb

pattern Node :: Exp Int -> Exp Int -> Exp BB -> Exp BVH
pattern Node l r bb = BVH True_ l r bb

vecToColor :: Exp V3f -> Exp G.Colour
vecToColor (V3_ x y z) = G.rgba x y z 1

hitInfoToColor :: Exp TriangleHitInfo -> Exp G.Colour
hitInfoToColor (TriangleHitInfo t u v ti) = ti & match \case 
    Nothing_ -> G.rgba 0 0 0 0
    Just_ _  -> G.rgba u v 0 0

transform :: Exp Float -> Exp Mat4f
transform time = identity `mtranslate` V3_ (time/10) 0 0

triangle :: Exp Float -> Exp DIM1 -> Exp Triangle
triangle t (I1 i) = tmul (mkTranslation (V3_ (divF i (-8)) (t/10) 0)) $ 
                  Triangle (V3_ 0 0 1) (V3_ 1 0 1) (V3_ 0.5 1 1)

mkScene :: Exp Float -> Acc (Vector Triangle, Vector BVH)
mkScene time = let
    ts :: Acc (Vector Triangle)
    ts = A.generate (I1 5) (triangle time)

    bvhlist = [BVH_ True 1 2 bbox, BVH_ True 3 4 bbox, BVH_ False 0 1 bbox, BVH_ False 1 1 bbox, BVH_ False 2 2 bbox]

    bvhs :: Acc (Vector BVH)
    bvhs = A.use $ A.fromList (Z :. P.length bvhlist) bvhlist

    in T2 ts bvhs;


genRay :: Exp Float -> Exp Float -> Exp Ray
genRay x y = 
    let
        target = V3_ x y 0
        origin = V3_ 0.5 0.5 (-1)
        direction = normalize (target - origin)
    in Ray origin direction

pnext1 :: Exp DIM1 -> Exp DIM1
pnext1 = pnextn 1

pnextn :: Exp Int -> Exp DIM1 -> Exp DIM1
pnextn d (I1 i) = I1 (d+i)

type ShortStack a = Stack ('NS ('NS ('NS ('NS ('NS 'NZ))))) a

liftAcc :: (Elt a, Elt b) => (Exp a -> Exp b) -> (Acc (Scalar a) -> Acc (Scalar b))
liftAcc f = unit . f . the

rangeLoop :: (Elt a) => Exp (Int, Int) -> (Exp Int -> Exp a -> Exp a) -> Exp a -> Exp a
rangeLoop (T2 start end) f initial = let

    whileCond (T2 i _) = i A.< end

    it (T2 i s) = T2 (i+1) (f i s)

    in A.snd $ A.while whileCond it (T2 start initial)


type WState = Exp (TriangleHitInfo, ShortStack BVH)

sceneIntersect :: Scene -> Acc (Vector Ray) -> Acc (Vector TriangleHitInfo)
sceneIntersect (T2 triangles bvh) rays =
    let
        startStack :: Exp (ShortStack BVH)
        startStack = stackPush emptyStack (bvh!I1 0)

        startHit :: Exp TriangleHitInfo
        startHit = TriangleHitInfo 10000 0 0 Nothing_

        initialWhileState :: WState
        initialWhileState = T2 startHit startStack

        whileCond :: Exp (TriangleHitInfo, ShortStack BVH) -> Exp Bool
        whileCond (T2 _ stack) = A.not (stackIsEmpty stack)

        itWhile :: Exp Ray -> Exp (TriangleHitInfo, ShortStack BVH) -> Exp (TriangleHitInfo, ShortStack BVH)
        itWhile ray (T2 hi stack) = let

            T2 node poppedstack = stackPop stack

            newstate :: Exp (TriangleHitInfo, ShortStack BVH)
            newstate = node & match \case
                Leaf start count _ -> let
                  whileCond :: Exp (Int, TriangleHitInfo) -> Exp Bool
                  whileCond (T2 i _) = i A.< (start + count)

                  itWhile :: Exp Int -> Exp TriangleHitInfo -> Exp TriangleHitInfo
                  itWhile i hi = let di = I1 i in closer hi (rayIntersect di (triangles!di) ray)

                  in T2 (rangeLoop (T2 start (start+count)) itWhile hi) poppedstack

                Node l r bbox -> let
                  bb = slabTest ray bbox hi
                  newstack = stackPush (stackPush poppedstack (bvh!I1 l)) (bvh!I1 r)
                  in T2 hi (bb ? (newstack, poppedstack))


            in stackIsEmpty stack A.? (T2 hi stack, newstate)

        in A.map (A.fst . \ray -> A.while whileCond (itWhile ray) initialWhileState) rays


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

{-
main :: IO ()
main = do
    let s :: Exp (ShortStack BVH)
        s = stackPush (stackPush emptyStack (Leaf 42 42)) (Leaf 69 69)
    print s
    let (T2 r s') = stackPop s
    print r
    let (T2 r s'') = stackPop s'
    print r
    let (T2 r s''') = stackPop s''
    print r
    return ()
-}
