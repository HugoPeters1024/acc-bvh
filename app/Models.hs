module Models where

import Codec.Wavefront
import Codec.Wavefront.IO
import qualified Data.Vector as V
import qualified Data.Array.Accelerate as A
import qualified Data.Array.Accelerate.Linear.V3 as V3
import LinAlg

bbox :: BB
bbox = BB_ (V3.V3 (-100) (-100) (-100)) (V3.V3 100 100 100)

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

    

constructBVH :: V.Vector Triangle -> V.Vector BVH
constructBVH triangles = V.fromList [BVH_ False 0 (V.length triangles) bbox]

toAcc' :: (A.Elt a) => V.Vector a -> A.Vector a
toAcc' v = A.fromFunction (A.Z A.:. V.length v) (\(A.Z A.:. i) -> v V.! i)

toAcc :: (A.Elt a) => V.Vector a -> A.Acc (A.Vector a)
toAcc = A.use . toAcc'

