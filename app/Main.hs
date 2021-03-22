{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE ImplicitParams #-}
{-# LANGUAGE RankNTypes #-}
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


type Scene = Acc (Vector Triangle)

type RayT a = (?scene :: Scene) => a

runRayT :: Scene -> RayT a -> a 
runRayT scene next = let ?scene = scene in next


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

triangles :: Exp Float -> Acc (Vector Triangle)
triangles time = A.generate (I1 1) triangle

genRay :: Exp Float -> Exp Float -> Exp Ray
genRay x y = 
    let
        target = V3_ x y 0
        origin = V3_ 0.5 0.5 (-1)
        direction = normalize (target - origin)
    in Ray origin direction

nrTriangles :: Scene -> Exp Int
nrTriangles = unindex1 . shape

pnext1 :: Exp DIM1 -> Exp DIM1
pnext1 (I1 i) = I1 (i+1)

sceneIntersect :: Scene -> Acc (Vector Ray) -> Acc (Vector TriangleHitInfo)
sceneIntersect scene rays =
    let
        initialWhileState :: Acc (Vector TriangleHitInfo, Scalar DIM1)
        initialWhileState = lift (A.map (const (TriangleHitInfo 0 0 0 Nothing_)) rays, unit (I1 0))

        whileCond :: Acc (Vector TriangleHitInfo, Scalar DIM1) -> Acc (Scalar Bool)
        whileCond (T2 _ c) = (unit . (A.< nrTriangles scene) . unindex1 . the) c 

        itWhile :: Acc (Vector TriangleHitInfo, Scalar DIM1) -> Acc (Vector TriangleHitInfo, Scalar DIM1)
        itWhile (T2 hs c) = let 

            i :: Exp DIM1
            i = the c
            -- Triangle to process in this wave
            triangle :: Exp Triangle
            triangle = scene ! i

            hitInfos :: Acc (Vector TriangleHitInfo)
            hitInfos = A.map (rayIntersect i triangle) rays

            in T2 (A.zipWith closer hs hitInfos) (unit (pnext1 i))

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
        scene = triangles 0
        pixels = A.map hitInfoToColor (sceneIntersect scene rays)
     in reshape (I2 640 480) pixels

main :: IO ()
main = G.playArrayWith
    PTX.run1
    (G.InWindow "BVH" (640, 480) (10,10))
    (1,1)
    60
    ()
    (const ())
    (const render)
    (const id)
    (const id)
