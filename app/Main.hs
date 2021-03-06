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
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE GADTs #-}
module Main where


import System.IO
import Prelude as P
import LinAlg
import qualified Graphics.Gloss as Gloss
import Data.Array.Accelerate.LLVM.PTX as PTX
import Data.Array.Accelerate.Interpreter as Interpreter
import Data.Array.Accelerate as A
import Data.Array.Accelerate.Error as A
--import Data.Array.Accelerate.Debug as A
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
import Control.DeepSeq (force)

import Models
import Stack
--import Structures

type Scene = Acc (Vector Triangle, Vector BVH)

windowWidth :: Int
windowWidth = 640
windowHeight :: Int
windowHeight = 480

ewindowWidth :: Exp Int
ewindowWidth = lift windowWidth

ewindowHeight :: Exp Int
ewindowHeight = lift windowHeight

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

saveSceneToFile :: V.Vector Triangle -> V.Vector BVH -> IO ()
saveSceneToFile ts bvh = do
    o <- openFile "scene.txt" WriteMode
    hPrint o ts
    hPrint o bvh

loadSceneFromFile :: String -> IO (V.Vector Triangle, V.Vector BVH)
loadSceneFromFile fname = do
    [tsData, bvhData] <- lines <$> readFile fname
    let ts = read tsData :: V.Vector Triangle
    let bvh = read bvhData :: V.Vector BVH
    return (ts, bvh)

    


loadScene :: IO Scene
loadScene = do
    (sortedTriangles, bvh) <- force <$> loadSceneFromFile "scene.txt"
    --triangles <- force <$> loadTriangles "florian_small.obj"
    putStrLn $ "nr triangles: " <> show (V.length sortedTriangles)
    --let (bvh, sortedTriangles) = force $ constructBVH triangles
    --saveSceneToFile sortedTriangles bvh
    putStrLn $ "nr bvh nodes: " <> show (V.length bvh)
    pure $ T2 (toAcc sortedTriangles) (toAcc bvh)


genRay :: Exp Float -> Exp Float -> Exp Float -> Exp Ray
genRay time x y = 
    let
        viewTarget :: Exp V3f = V3_ 0 1 0
        angle :: Exp Float = time / 4
        viewDist :: Exp Float = 80
        eye = V3_ (viewDist * cos angle) 1 (viewDist * sin angle)
        viewDir = normalize $ viewTarget - eye
        dist = 1.8
        screenCenter = eye + viewDir * dist
        screenHorz = V3.cross (V3_ 0 1 0) viewDir
        screenVert = V3.cross screenHorz viewDir
        target = screenCenter + (screenHorz A.* 2 A.* V3_ x x x) + (screenVert A.* 2 A.* V3_ y y y) - screenHorz - screenVert
        direction = normalize (target - eye)
    in Ray eye direction

pnext1 :: Exp DIM1 -> Exp DIM1
pnext1 = pnextn 1

pnextn :: Exp Int -> Exp DIM1 -> Exp DIM1
pnextn d (I1 i) = I1 (d+i)

liftAcc :: (Elt a, Elt b) => (Exp a -> Exp b) -> (Acc (Scalar a) -> Acc (Scalar b))
liftAcc f = unit . f . the

rangeLoop :: (Elt a) => Exp (Int, Int) -> (Exp Int -> Exp a -> Exp a) -> Exp a -> Exp a
rangeLoop (T2 start end) f initial = let
    whileCond (T2 i _) = i A.< end
    it (T2 i s) = T2 (i+1) (f i s)
    in A.snd $ A.while whileCond it (T2 start initial)

type ShortStack = Stack 15 Int16
type WState = Exp (TriangleHitInfo, ShortStack)

sceneIntersect :: Scene -> Acc (Vector Ray) -> Acc (Vector TriangleHitInfo)
sceneIntersect (T2 triangles bvh) rays =
    let
        startStack :: Exp ShortStack
        startStack = stackPush emptyStack 0

        startHit :: Exp TriangleHitInfo
        startHit = TriangleHitInfo 10000 0 0 Nothing_

        initialWhileState :: WState
        initialWhileState = T2 startHit startStack

        whileCond :: Exp (TriangleHitInfo, ShortStack) -> Exp Bool
        whileCond (T2 _ stack) = A.not (stackIsEmpty stack)

        itWhile :: Exp Ray -> Exp (TriangleHitInfo, ShortStack) -> Exp (TriangleHitInfo, ShortStack)
        itWhile ray (T2 hi stack) = let

            T2 poppedstack node :: Exp (ShortStack, BVH) = let T2 stack' nodeIdx = stackPop stack in T2 stack' (bvh ! I1 (A.fromIntegral nodeIdx))

            newstate :: Exp (TriangleHitInfo, ShortStack)
            newstate = node & match \case
                Leaf start count _ -> let
                  itWhile :: Exp Int -> Exp TriangleHitInfo -> Exp TriangleHitInfo
                  itWhile i hi = let di = I1 i in closer hi (rayIntersect di (triangles!di) ray)

                  in T2 (rangeLoop (T2 start (start+count)) itWhile hi) poppedstack

                Node l r bbox -> let
                  bb = slabTest ray bbox hi
                  newstack :: Exp ShortStack = stackPush (stackPush poppedstack (A.fromIntegral l)) (A.fromIntegral r)
                  in T2 hi (bb ? (newstack, poppedstack))


            in stackIsEmpty stack A.? (T2 hi stack, newstate)

        in A.map (A.fst . \ray -> A.while whileCond (itWhile ray) initialWhileState) rays


rayIntersect :: Exp DIM1 -> Exp Triangle -> Exp Ray -> Exp TriangleHitInfo
rayIntersect i triangle ray = rayTriangleIsect triangle ray & match \case
    Nothing_ -> TriangleHitInfo 0 0 0 Nothing_
    Just_ (T3 t u v) -> TriangleHitInfo t u v (Just_ i)

initialRays :: Exp Float -> Acc (Vector Ray)
initialRays time = A.flatten $ A.generate (I2 ewindowHeight ewindowWidth) (\(I2 y x) -> genRay time (divF x ewindowWidth) (divF y ewindowHeight))

white :: Exp G.Colour
white = G.rgba 1 1 1 1

divF :: Exp Int -> Exp Int -> Exp Float
divF a b = A.fromIntegral a / A.fromIntegral b

render :: Scene -> Exp Float -> Acc (Matrix G.Colour)
render scene time = let
        rays = initialRays time
        pixels = A.map (hitInfoToColor scene) (sceneIntersect scene rays)
        in reshape (I2 ewindowHeight ewindowWidth) pixels


runExp :: Elt e => Exp e -> e
runExp e = indexArray (PTX.run (generate I0 (const e))) A.Z
--runExp e = indexArray (PTX.run (unit e)) A.Z

main :: IO ()
main = do
    let v :: Exp (Vec 3 Int) = vecWrite (vecWrite (vecWrite vecEmpty 0 69) 1 420) 2 42
    putStrLn $ "vec[0]: " <> show (runExp (vecIndex v 0))
    putStrLn $ "vec[1]: " <> show (runExp (vecIndex v 1))
    putStrLn $ "vec[2]: " <> show (runExp (vecIndex v 2))
    scene <- loadScene
    G.animateArrayWith PTX.run1 (G.InWindow "BVH" (windowWidth, windowHeight) (10,10)) (1,1) (render scene)

