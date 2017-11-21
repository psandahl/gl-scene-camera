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
    , position
    , forward
    , backward
    , up
    , down
    , turnLeft
    , turnRight
    , viewLeft
    , viewRight
    , viewUp
    , viewDown
    ) where

import           Control.Lens (makeLenses, (%~), (.~), (^.))
import           Flow         ((<|))
import           Graphics.GL  (GLfloat)
import           Linear       (M44, V3, (*^), _y)
import           Scene.Math   (Angle (..), addAngles, clamp, fromEulerAngles,
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
    position .~
        moveTo (camera ^. position) (camera ^. moveVector) distance <| camera
{-# INLINE forward #-}

-- | Move the 'Camera' backward by the specified distance using the negated
-- move direction for the camera.
backward :: GLfloat -> Camera -> Camera
backward distance camera =
    position .~
        moveTo (camera ^. position) (negate <| camera ^. moveVector) distance <| camera
{-# INLINE backward #-}

-- | Lift the camera's height position up with the specified amount.
up :: GLfloat -> Camera -> Camera
up distance = (position . _y) %~ (distance +)
{-# INLINE up #-}

-- | Lower the camera's height position down with the specified amount.
down :: GLfloat -> Camera -> Camera
down distance = up (negate distance)
{-# INLINE down #-}

-- | Turn the 'Camera's view and move angles to the left.
turnLeft :: Angle GLfloat -> Camera -> Camera
turnLeft = turn
{-# INLINE turnLeft #-}

-- | Turn the 'Camera's view and move angles to the right.
turnRight :: Angle GLfloat -> Camera -> Camera
turnRight theta = turn (negateAngle theta)
{-# INLINE turnRight #-}

-- | Turn the 'Camera's view angles to the left.
viewLeft :: Angle GLfloat -> Camera -> Camera
viewLeft = changeViewHeading
{-# INLINE viewLeft #-}

-- | Turn the 'Camera's view angles to the right.
viewRight :: Angle GLfloat -> Camera -> Camera
viewRight theta = changeViewHeading (negateAngle theta)
{-# INLINE viewRight #-}

viewUp :: Angle GLfloat -> Camera -> Camera
viewUp = changeViewElevation
{-# INLINE viewUp #-}

viewDown :: Angle GLfloat -> Camera -> Camera
viewDown theta = changeViewElevation (negateAngle theta)
{-# INLINE viewDown #-}

turn :: Angle GLfloat -> Camera -> Camera
turn theta = changeViewHeading theta . changeMoveHeading theta

changeViewElevation :: Angle GLfloat -> Camera -> Camera
changeViewElevation theta camera =
    let viewDirection' = elevation %~ clampElevation . addAngles theta <| camera ^. viewDirection
    in viewDirection .~ viewDirection' <|
        viewVector .~ fromDirection viewDirection' <|
        camera

changeViewHeading :: Angle GLfloat -> Camera -> Camera
changeViewHeading theta camera =
    let viewDirection' = heading %~ addAngles theta <| camera ^. viewDirection
    in viewDirection .~ viewDirection' <|
        viewVector .~ fromDirection viewDirection' <|
        camera

changeMoveHeading :: Angle GLfloat -> Camera -> Camera
changeMoveHeading theta camera =
    let moveDirection' = heading %~ addAngles theta <| camera ^. moveDirection
    in moveDirection .~ moveDirection' <|
        moveVector .~ fromDirection moveDirection' <|
        camera

fromDirection :: Direction -> V3 GLfloat
fromDirection direction =
    fromEulerAngles (direction ^. heading) (direction ^. elevation)

moveTo :: V3 GLfloat -> V3 GLfloat -> GLfloat -> V3 GLfloat
moveTo position' direction distance =
    position' + (distance *^ direction)

vista :: GLfloat
vista = 10

clampElevation :: Angle GLfloat -> Angle GLfloat
clampElevation (Degrees theta) = Degrees <| clamp (-89) 89 theta
clampElevation (Radians theta) = Radians <| clamp (-pi / 2 - 0.01) (pi / 2 - 0.01) theta
