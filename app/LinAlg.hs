{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE BlockArguments #-}
module LinAlg where

import Data.Primitive
import qualified Prelude as P hiding ((<), (>), (&&))
import Data.Array.Accelerate as A
import Data.Array.Accelerate.Sugar.Elt as A
import Data.Array.Accelerate.Linear.V3 as V3
import Data.Array.Accelerate.Linear.V4 as V4
import Data.Array.Accelerate.Linear.Matrix as M
import Data.Array.Accelerate.Linear.Metric

type V3f = V3 Float
type Mat4f = M44 Float

zipComponents :: (Exp Float -> Exp Float -> Exp Float) -> Exp V3f -> Exp V3f -> Exp V3f
zipComponents f (V3_ a0 a1 a2) (V3_ b0 b1 b2) = V3_ (f a0 b0) (f a1 b1) (f a2 b2)

min3f :: Exp V3f -> Exp V3f -> Exp V3f
min3f = zipComponents min

max3f :: Exp V3f -> Exp V3f -> Exp V3f
max3f = zipComponents max

foldComponents :: (Exp Float -> Exp Float -> Exp Float) -> Exp V3f -> Exp Float
foldComponents f (V3_ v0 v1 v2) = f v0 (f v1 v2)

maxComponent :: Exp V3f -> Exp Float
maxComponent = foldComponents max

minComponent :: Exp V3f -> Exp Float
minComponent = foldComponents min

class Transformable a where
    tmul :: Exp Mat4f -> Exp a -> Exp a

data Ray = Ray_ V3f V3f
    deriving (Generic, Elt, Show)

pattern Ray :: Exp V3f -> Exp V3f -> Exp Ray
pattern Ray o d = A.Pattern (o, d)

getRayOrigin :: Exp Ray -> Exp V3f
getRayOrigin (Ray o _) = o

getRayDirection :: Exp Ray -> Exp V3f
getRayDirection (Ray _ d) = d

mtranslate :: Exp Mat4f -> Exp V3f -> Exp Mat4f
mtranslate (V4_
    (V4_ v00 v01 v02 v03)
    (V4_ v10 v11 v12 v13)
    (V4_ v20 v21 v22 v23)
    (V4_ v30 v31 v32 v33))
    (V3_ tx ty tz) = V4_ 
        (V4_ v00 v01 v02 (v03+tx))
        (V4_ v10 v11 v12 (v13+ty))
        (V4_ v20 v21 v22 (v23+tz))
        (V4_ v30 v31 v32 v33)

mkRotationY :: Exp Float -> Exp Mat4f
mkRotationY r = V4_
    (V4_ (cos r)    0 (sin r) 0)
    (V4_ 0          1 0       0)
    (V4_ (-(sin r)) 0 (cos r) 1)
    (V4_ 0          0 0       1)

mkRotationZ :: Exp Float -> Exp Mat4f
mkRotationZ r = V4_
    (V4_ (cos r)    (-(sin r)) 0 0)
    (V4_ (sin r)    (cos r)    0 0)
    (V4_ 0          0          1 0)
    (V4_ 0          0          0 1)

mkTranslation :: Exp V3f -> Exp Mat4f
mkTranslation = mtranslate identity

data Triangle = Triangle_ V3f V3f V3f
    deriving (Generic, Elt, Show)

pattern Triangle :: Exp V3f -> Exp V3f -> Exp V3f -> Exp Triangle
pattern Triangle v0 v1 v2 = A.Pattern (v0, v1, v2)

getNormal :: Exp Triangle -> Exp V3f
getNormal (Triangle v0 v1 v2) = let
    edge1 = v1 - v0
    edge2 = v2 - v0
    in normalize $ V3.cross edge1 edge2

instance Transformable V3f where
  tmul m (V3_ x y z) = let (V4_ nx ny nz _) = m !* V4_ x y z 1 in V3_ nx ny nz

instance Transformable Triangle where
    tmul m (Triangle v0 v1 v2) = Triangle (tmul m v0) (tmul m v1) (tmul m v2)

data TriangleHitInfo = TriangleHitInfo_
    { t :: Float
    , u :: Float
    , v :: Float
    , i :: Maybe DIM1
    } deriving (Generic, Elt, Show)

pattern TriangleHitInfo :: Exp Float -> Exp Float -> Exp Float -> Exp (Maybe DIM1) -> Exp TriangleHitInfo
pattern TriangleHitInfo t u v i = A.Pattern (t, u, v, i)

closer :: Exp TriangleHitInfo -> Exp TriangleHitInfo -> Exp TriangleHitInfo
closer a@(TriangleHitInfo ta _ _ ia) b@(TriangleHitInfo tb _ _ ib) = T2 ia ib & match \case
    (T2 Nothing_ _) -> b
    (T2 _ Nothing_) -> a
    (T2 (Just_ _) (Just_ _)) -> ta A.< tb A.? (a , b)

setT :: Exp Float -> Exp TriangleHitInfo -> Exp TriangleHitInfo
setT t (TriangleHitInfo _ a b c) = TriangleHitInfo t a b c


rayTriangleIsect :: Exp Triangle -> Exp Ray -> Exp (Maybe (Float, Float, Float))
rayTriangleIsect (Triangle v0 v1 v2) (Ray o d) = 
    let
        v0v1 = v1 - v0
        v0v2 = v2 - v0
        pvec = d `cross` v0v2
        det = v0v1 `dot` pvec
        invDet = 1.0 / det

        tvec = o - v0
        u = (tvec `dot` pvec) * invDet

        qvec = tvec `cross` v0v1
        v = (d `dot` qvec) * invDet

        t = (v0v2 `dot` qvec) * invDet
    in (P.abs det > 0.0001 && t > 0 && u > 0 && v > 0 && u + v < 1) 
       ? (Just_ (T3 t u v), Nothing_)

data BB = BB_ V3f V3f
    deriving (Generic, Elt, Show)

pattern BB :: Exp V3f -> Exp V3f -> Exp BB
pattern BB vmin vmax = A.Pattern (vmin, vmax)

slabTest :: Exp Ray -> Exp BB -> Exp TriangleHitInfo -> Exp Bool
slabTest (Ray o d) (BB vmin vmax) (TriangleHitInfo t _ _ _) = let
    invd :: Exp V3f
    invd = 1.0 / o

    t0 = (vmin - o) * invd
    t1 = (vmax - o) * invd
    min3 = min3f t0 t1
    max3 = max3f t0 t1
    tmin = maxComponent min3
    tmax = minComponent max3
    in True_ --(tmax A.>= max 0 tmin) A.&& (tmin A.< t)


data BVH = BVH_ Bool Int Int BB
    deriving (Generic, Elt, Show)


pattern BVH :: Exp Bool -> Exp Int -> Exp Int -> Exp BB -> Exp BVH
pattern BVH leaf l r bb = A.Pattern (leaf, l, r, bb)

pattern Leaf :: Exp Int -> Exp Int -> Exp BB -> Exp BVH
pattern Leaf l r bb = BVH False_ l r bb

pattern Node :: Exp Int -> Exp Int -> Exp BB -> Exp BVH
pattern Node l r bb = BVH True_ l r bb
