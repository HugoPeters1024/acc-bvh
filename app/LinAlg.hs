{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE BlockArguments #-}
module LinAlg where

import qualified Prelude as P hiding ((<), (>), (&&))
import Data.Array.Accelerate as A
import Data.Array.Accelerate.Linear.V3 as V3
import Data.Array.Accelerate.Linear.V4 as V4
import Data.Array.Accelerate.Linear.Matrix as M
import Data.Array.Accelerate.Linear.Metric

type V3f = V3 Float
type Mat4f = M44 Float

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

data Triangle = Triangle_ V3f V3f V3f
    deriving (Generic, Elt, Show)

pattern Triangle :: Exp V3f -> Exp V3f -> Exp V3f -> Exp Triangle
pattern Triangle v0 v1 v2 = A.Pattern (v0, v1, v2)

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
