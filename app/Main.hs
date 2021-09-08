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
import Data.Array.Accelerate.IO.Data.Vector.Generic
import qualified Data.Array.Accelerate.Unsafe as A
import qualified Graphics.Gloss.Accelerate.Raster.Array as G
import Data.Array.Accelerate.Linear.V3 as V3
import Data.Array.Accelerate.Linear.Metric
import Data.Array.Accelerate.Linear.Matrix
import Control.Monad.Trans.Reader
import Control.Monad.Identity
import GHC.Generics
import GHC.TypeNats
import qualified Data.Vector as V

import Structures
import Models

type Scene = Acc (Vector Triangle, Vector BVH)

vecToColor :: Exp V3f -> Exp G.Colour
vecToColor (V3_ x y z) = G.rgba x y z 1

hitInfoToColor :: Scene -> Exp TriangleHitInfo -> Exp G.Colour
hitInfoToColor (T2 triangles _) (TriangleHitInfo t u v ti) = ti & match \case 
    Nothing_ -> G.rgba 0.1 0.2 0.2 0
    Just_ i  -> let
        triangle = triangles A.! i
        normal = getNormal triangle
        angle = dot normal (V3.V3_ 0 1 0)
        in G.rgba angle angle angle angle

transform :: Exp Float -> Exp Mat4f
transform time = identity `mtranslate` V3_ (time/10) 0 0

cube :: Vector Triangle
cube = A.fromList (A.Z :. 12)
  [ Triangle_ (V3.V3 (-1.0) (-1.0) (-1.0)) (V3.V3 (-1.0) (-1.0) ( 1.0)) (V3.V3 (-1.0) ( 1.0) ( 1.0)) 
  , Triangle_ (V3.V3 ( 1.0) ( 1.0) (-1.0)) (V3.V3 (-1.0) (-1.0) (-1.0)) (V3.V3 (-1.0) ( 1.0) (-1.0)) 
  , Triangle_ (V3.V3 ( 1.0) (-1.0) ( 1.0)) (V3.V3 (-1.0) (-1.0) (-1.0)) (V3.V3 ( 1.0) (-1.0) (-1.0)) 
  , Triangle_ (V3.V3 ( 1.0) ( 1.0) (-1.0)) (V3.V3 ( 1.0) (-1.0) (-1.0)) (V3.V3 (-1.0) (-1.0) (-1.0)) 
  , Triangle_ (V3.V3 (-1.0) (-1.0) (-1.0)) (V3.V3 (-1.0) ( 1.0) ( 1.0)) (V3.V3 (-1.0) ( 1.0) (-1.0)) 
  , Triangle_ (V3.V3 ( 1.0) (-1.0) ( 1.0)) (V3.V3 (-1.0) (-1.0) ( 1.0)) (V3.V3 (-1.0) (-1.0) (-1.0)) 
  , Triangle_ (V3.V3 (-1.0) ( 1.0) ( 1.0)) (V3.V3 (-1.0) (-1.0) ( 1.0)) (V3.V3 ( 1.0) (-1.0) ( 1.0)) 
  , Triangle_ (V3.V3 ( 1.0) ( 1.0) ( 1.0)) (V3.V3 ( 1.0) (-1.0) (-1.0)) (V3.V3 ( 1.0) ( 1.0) (-1.0)) 
  , Triangle_ (V3.V3 ( 1.0) (-1.0) (-1.0)) (V3.V3 ( 1.0) ( 1.0) ( 1.0)) (V3.V3 ( 1.0) (-1.0) ( 1.0)) 
  , Triangle_ (V3.V3 ( 1.0) ( 1.0) ( 1.0)) (V3.V3 ( 1.0) ( 1.0) (-1.0)) (V3.V3 (-1.0) ( 1.0) (-1.0)) 
  , Triangle_ (V3.V3 ( 1.0) ( 1.0) ( 1.0)) (V3.V3 (-1.0) ( 1.0) (-1.0)) (V3.V3 (-1.0) ( 1.0) ( 1.0)) 
  , Triangle_ (V3.V3 ( 1.0) ( 1.0) ( 1.0)) (V3.V3 (-1.0) ( 1.0) ( 1.0)) (V3.V3 ( 1.0) (-1.0) ( 1.0))]

loadScene :: IO Scene
loadScene = do
    triangles <- loadTriangles "teapot.obj"
    putStrLn $ "nr triangles: " <> show (V.length triangles)
    let (bvh, sortedTriangles) = constructBVH triangles
    putStrLn $ "nr bvh nodes: " <> show (V.length bvh)
    pure $ T2 (toAcc sortedTriangles) (toAcc bvh)


genRay :: Exp Float -> Exp Float -> Exp Float -> Exp Ray
genRay time x y = 
    let
        eye = V3_ 0 (time * 0.2) (-6)
        viewDir = V3_ 0 0 1
        dist = 1.0
        screenCenter = eye + viewDir * dist
        screenHorz = V3.cross (V3_ 0 1 0) viewDir
        screenVert = V3.cross screenHorz viewDir
        target = screenCenter + (screenHorz A.* 2 A.* V3_ x x x) + (screenVert A.* 2 A.* V3_ y y y) - screenHorz - screenVert
        --target = V3_ x y (-9)
        direction = normalize (target - eye)
    in Ray eye direction

pnext1 :: Exp DIM1 -> Exp DIM1
pnext1 = pnextn 1

pnextn :: Exp Int -> Exp DIM1 -> Exp DIM1
pnextn d (I1 i) = I1 (d+i)

type ShortStack a = Stack ('NS ('NS ('NS ('NS ('NS ('NS ('NS 'NZ))))))) a

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

initialRays :: Exp Float -> Acc (Vector Ray)
initialRays time = A.flatten $ A.generate (I2 480 640) (\(I2 y x) -> genRay time (divF x 640) (divF y 480))

white :: Exp G.Colour
white = G.rgba 1 1 1 1

divF :: Exp Int -> Exp Int -> Exp Float
divF a b = A.fromIntegral a / A.fromIntegral b

render :: Scene -> Exp Float -> Acc (Array DIM2 G.Colour)
render scene time = let
        rays = initialRays time
        pixels = A.map (hitInfoToColor scene) (sceneIntersect scene rays)
        in reshape (I2 480 640) pixels

    

main :: IO ()
main = do
    scene <- loadScene
    G.animateArrayWith PTX.run1 (G.InWindow "BVH" (640, 480) (10,10)) (1,1) (render scene)

