-- |
-- Module: Scene.Camera
-- Copyright: (c) 2017 Patrik Sandahl
-- Licence: MIT
-- Maintainer: Patrik Sandahl <patrik.sandahl@gmail.com>
-- Stability: experimental
-- Portability: portable
module Scene.Camera
    ( Camera
    , mkCamera
    , matrix
    , fromEulerAngles -- TODO: remove from export.
    ) where

import           Flow        ((<|))
import           Graphics.GL (GLfloat)
import           Linear      (M44, V3, normalize, (!*!), (*^))
import           Scene.Math  (Angle (..), Application (..), apply, back3d,
                              eulerElevation, eulerHeading, mkRotationMatrix,
                              mkViewMatrix, negateAngle, up3d, x3d, y3d)

-- | Camera record. Opaque to the user.
data Camera = Camera
    { position      :: !(V3 GLfloat)
    , viewDirection :: !Direction
    , moveDirection :: !Direction
    } deriving Show

data Direction = Direction
    { vector    :: !(V3 GLfloat)
    , heading   :: !(Angle GLfloat)
    , elevation :: !(Angle GLfloat)
    } deriving Show

-- | Make a new 'Camera' from a position, a 'V3' telling the direction the
-- camera is viewing, and a 'V3' telling the direction the camera is moving.
mkCamera :: V3 GLfloat -> V3 GLfloat -> V3 GLfloat -> Camera
mkCamera position' viewDirection' moveDirection' =
    Camera
        { position = position'
        , viewDirection = fromVector <| normalize viewDirection'
        , moveDirection = fromVector <| normalize moveDirection'
        }

-- | From the 'Camera', produce a view matrix.
matrix :: Camera -> M44 GLfloat
matrix camera =
    mkViewMatrix (position camera)
                 (viewspot (position camera) (viewDirection camera))
                 up3d
{-# INLINE matrix #-}

fromVector :: V3 GLfloat -> Direction
fromVector vec =
    Direction
        { vector = vec
        , heading = eulerHeading vec
        , elevation = eulerElevation vec
        }

fromEulerAngles :: Angle GLfloat -> Angle GLfloat -> V3 GLfloat
fromEulerAngles heading' elevation' =
    let pitch = mkRotationMatrix x3d <| negateAngle elevation'
        yaw = mkRotationMatrix y3d heading'
        mat = yaw !*! pitch
    in apply mat <| Vector back3d

viewspot :: V3 GLfloat -> Direction -> V3 GLfloat
viewspot position' direction =
    position' + (vista *^ vector direction)

vista :: GLfloat
vista = 10
