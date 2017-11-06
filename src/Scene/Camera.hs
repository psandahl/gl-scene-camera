-- |
-- Module: Scene.Camera
-- Copyright: (c) 2017 Patrik Sandahl
-- Licence: MIT
-- Maintainer: Patrik Sandahl <patrik.sandahl@gmail.com>
-- Stability: experimental
-- Portability: portable
module Scene.Camera
    ( Camera
    , Direction (..)
    , mkCamera
    , matrix
    , forward
    ) where

import           Graphics.GL (GLfloat)
import           Linear      (M44, V3, (*^))
import           Scene.Math  (Angle (..), fromEulerAngles, mkViewMatrix, up3d)

-- | Camera record. Opaque to the user.
data Camera = Camera
    { position      :: !(V3 GLfloat)
    , viewDirection :: !Direction
    , viewVector    :: !(V3 GLfloat)
    , moveDirection :: !Direction
    , moveVector    :: !(V3 GLfloat)
    } deriving Show

-- | Direction record:
-- heading is an 'Angle' on the earth surface. Zero angle is pointing in
-- the positive Z direction.
-- elevation is an 'Angle' with respect to the horizon. Zero angle is aligned
-- with the horizon, a positive value points above and a negative below.
data Direction = Direction
    { heading   :: !(Angle GLfloat)
    , elevation :: !(Angle GLfloat)
    } deriving Show

-- | Make a camera from a 'V3' telling its initial position and two 'Direction'.
-- The first is the view direction and the other is the move direction.
mkCamera :: V3 GLfloat -> Direction -> Direction -> Camera
mkCamera position' viewDirection' moveDirection' =
    Camera
        { position = position'
        , viewDirection = viewDirection'
        , viewVector = fromDirection viewDirection'
        , moveDirection = moveDirection'
        , moveVector = fromDirection moveDirection'
        }

-- | From the 'Camera', produce a view matrix.
matrix :: Camera -> M44 GLfloat
matrix camera =
    mkViewMatrix (position camera)
                 (moveTo (position camera) (viewVector camera) vista)
                 up3d
{-# INLINE matrix #-}

-- | Move the 'Camera' forward by the specified distance in the
-- the camera's move direction.
forward :: GLfloat -> Camera -> Camera
forward distance camera =
    camera
        { position = moveTo (position camera) (moveVector camera) distance
        }
{-# INLINE forward #-}

fromDirection :: Direction -> V3 GLfloat
fromDirection direction =
    fromEulerAngles (heading direction) (elevation direction)

moveTo :: V3 GLfloat -> V3 GLfloat -> GLfloat -> V3 GLfloat
moveTo position' direction distance =
    position' + (distance *^ direction)

vista :: GLfloat
vista = 10
