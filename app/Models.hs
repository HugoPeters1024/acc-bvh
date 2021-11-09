module Models where

import Debug.Trace
import Codec.Wavefront
import Codec.Wavefront.IO
import Control.Monad.State
import qualified Data.Vector as V
import qualified Data.Vector.Algorithms.Intro as V
import qualified Data.Array.Accelerate as A
import qualified Data.Array.Accelerate.Linear.V3 as V3
import Data.Array.Accelerate.Linear.V3
import LinAlg

bbox :: BB
bbox = BB_ (V3.V3 (-100) (-100) (-100)) (V3.V3 100 100 100)

min3f_ :: V3f -> V3f -> V3f
min3f_ (V3 xl yl zl) (V3 xr yr zr) = V3 (min xl xr) (min yl yr) (min zl zr)

max3f_ :: V3f -> V3f -> V3f
max3f_ (V3 xl yl zl) (V3 xr yr zr) = V3 (max xl xr) (max yl yr) (max zl zr)

triangleBB :: Triangle -> BB
triangleBB (Triangle_ v0 v1 v2) = BB_ (min3f_ (min3f_ v0 v1) v2) (max3f_ (max3f_ v0 v1) v2)

triangleC :: Triangle -> V3f
triangleC (Triangle_ v0 v1 v2) = 0.33333333 * (v0 + v1 + v2)

grow :: BB -> BB -> BB
grow (BB_ vminl vmaxl) (BB_ vminr vmaxr) = BB_ (min3f_ vminl vminr) (max3f_ vmaxl vmaxr)

svx :: V3f -> V3f -> Ordering
svx (V3.V3 xl _ _) (V3.V3 xr _ _) = compare xl xr

svy :: V3f -> V3f -> Ordering
svy (V3.V3 _ yl  _) (V3.V3 _ yr _) = compare yl yr

svz :: V3f -> V3f -> Ordering
svz (V3.V3 _ _ zl) (V3.V3 _ _ zr) = compare zl zr

st :: (V3f -> V3f -> Ordering) -> Triangle -> Triangle -> Ordering
st f t0 t1 = f (triangleC t0) (triangleC t1)


data Axis = X | Y | Z deriving Show

dominantAxisBB :: BB -> Axis
dominantAxisBB (BB_ vmin vmax) = let V3.V3 dx dy dz = vmax - vmin in if dx > dy && dx > dz then X else if dy > dz then Y else Z


loadTriangles :: FilePath -> IO (V.Vector Triangle)
loadTriangles path = do
    Right wave <- fromFile path
    let locations = objLocations wave
    let faces = V.map elValue (objFaces wave)
    let convert (Face i1 i2 i3 _) = let
            Location x1 y1 z1 _ = locations V.! (faceLocIndex i1 - 1) 
            Location x2 y2 z2 _ = locations V.! (faceLocIndex i2 - 1)
            Location x3 y3 z3 _ = locations V.! (faceLocIndex i3 - 1)
            in Triangle_ (V3.V3 x1 y1 z1) (V3.V3 x2 y2 z2) (V3.V3 x3 y3 z3)

    pure $ V.map convert faces


trace' :: Show a => a -> a
trace' x = trace (show x) x
    

constructBVH :: V.Vector Triangle -> (V.Vector BVH, V.Vector Triangle)
constructBVH triangles = let

    gen :: V.Vector Triangle -> (Int, Int) -> Int -> (V.Vector BVH, V.Vector Triangle)
    gen allTriangles (start, count) idx = let
        triangles = V.take count (V.drop start allTriangles)
        gbox = V.foldl1 grow (V.map triangleBB triangles)

        in if count < 50
              then (V.fromList [BVH_ False start count gbox], allTriangles)
              else let
                    trianglesSorted = case dominantAxisBB gbox of
                            X -> V.modify (V.sortBy (st svx)) triangles
                            Y -> V.modify (V.sortBy (st svy)) triangles
                            Z -> V.modify (V.sortBy (st svz)) triangles
                    newTriangles = V.update allTriangles (V.zip (V.generate count (+start)) trianglesSorted)
                    countLeft = count `div` 2
                    countRight = count - countLeft
                    leftIdx = idx + 1
                    (left, tr) = gen newTriangles (start, countLeft) leftIdx
                    rightIdx = leftIdx + V.length left
                    (right, tr') = gen tr (start + countLeft, countRight) rightIdx
                    in (V.cons (BVH_ True leftIdx rightIdx gbox) (left V.++ right), tr')
    in gen triangles (0, V.length triangles) 0


toAcc' :: (A.Elt a) => V.Vector a -> A.Vector a
toAcc' v = A.fromFunction (A.Z A.:. V.length v) (\(A.Z A.:. i) -> v V.! i)

toAcc :: (A.Elt a) => V.Vector a -> A.Acc (A.Vector a)
toAcc = A.use . toAcc'

