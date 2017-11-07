{-# LANGUAGE TemplateHaskell #-}
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
    , backward
    , turnLeft
    , turnRight
    ) where

import           Control.Lens (makeLenses, over, set, view, (%~), (.~), (^.))
import           Flow         ((<|))
import           Graphics.GL  (GLfloat)
import           Linear       (M44, V3, (*^))
import           Scene.Math   (Angle (..), addAngles, fromEulerAngles,
                               mkViewMatrix, negateAngle, up3d)

-- | Camera record. Opaque to the user.
data Camera = Camera
    { _position      :: !(V3 GLfloat)
    , _viewDirection :: !Direction
    , _viewVector    :: !(V3 GLfloat)
    , _moveDirection :: !Direction
    , _moveVector    :: !(V3 GLfloat)
    } deriving Show

-- | Direction record:
-- heading is an 'Angle' on the earth surface. Zero angle is pointing in
-- the positive Z direction.
-- elevation is an 'Angle' with respect to the horizon. Zero angle is aligned
-- with the horizon, a positive value points above and a negative below.
data Direction = Direction
    { _heading   :: !(Angle GLfloat)
    , _elevation :: !(Angle GLfloat)
    } deriving Show

makeLenses ''Camera
makeLenses ''Direction

-- | Make a camera from a 'V3' telling its initial position and two 'Direction'.
-- The first is the view direction and the other is the move direction.
mkCamera :: V3 GLfloat -> Direction -> Direction -> Camera
mkCamera position' viewDirection' moveDirection' =
    Camera
        { _position = position'
        , _viewDirection = viewDirection'
        , _viewVector = fromDirection viewDirection'
        , _moveDirection = moveDirection'
        , _moveVector = fromDirection moveDirection'
        }

-- | From the 'Camera', produce a view matrix.
matrix :: Camera -> M44 GLfloat
matrix camera =
    mkViewMatrix (camera ^. position)
                 (moveTo (camera ^. position) (camera ^. viewVector) vista)
                 up3d
{-# INLINE matrix #-}

-- | Move the 'Camera' forward by the specified distance in the
-- the camera's move direction.
forward :: GLfloat -> Camera -> Camera
forward distance camera =
    set position
        (moveTo (camera ^. position)
                (camera ^. moveVector) distance)
        camera
{-# INLINE forward #-}

-- | Move the 'Camera' backward by the specified distance using the negated
-- move direction for the camera.
backward :: GLfloat -> Camera -> Camera
backward distance camera =
    set position
        (moveTo (camera ^. position)
                (negate <| camera ^. moveVector) distance)
        camera
{-# INLINE backward #-}

turnLeft :: Angle GLfloat -> Camera -> Camera
turnLeft = turn
{-# INLINE turnLeft #-}

turnRight :: Angle GLfloat -> Camera -> Camera
turnRight theta = turn (negateAngle theta)
{-# INLINE turnRight #-}

turn :: Angle GLfloat -> Camera -> Camera
turn theta camera =
    let viewDirection' = over heading (addAngles theta) <| view viewDirection camera
        moveDirection' = over heading (addAngles theta) <| view moveDirection camera
    in set viewDirection viewDirection' <|
        set moveDirection moveDirection' <|
        set viewVector (fromDirection viewDirection') <|
        set moveVector (fromDirection moveDirection') camera

fromDirection :: Direction -> V3 GLfloat
fromDirection direction =
    fromEulerAngles (direction ^. heading) (direction ^. elevation)

moveTo :: V3 GLfloat -> V3 GLfloat -> GLfloat -> V3 GLfloat
moveTo position' direction distance =
    position' + (distance *^ direction)

vista :: GLfloat
vista = 10
