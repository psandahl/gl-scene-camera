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
    ) where

import           Graphics.GL (GLfloat)
import           Linear      (M44, V3, (*^))
import           Scene.Math  (mkViewMatrix, up3d)

data Direction = Direction
    { vector :: !(V3 GLfloat)
    } deriving Show

data Camera = Camera
    { moveDirection :: !Direction
    , viewDirection :: !Direction
    , position      :: !(V3 GLfloat)
    } deriving Show

mkCamera :: V3 GLfloat -> V3 GLfloat -> V3 GLfloat -> Camera
mkCamera = undefined

matrix :: Camera -> M44 GLfloat
matrix camera =
    mkViewMatrix (position camera)
                 (viewspot (position camera) (viewDirection camera))
                 up3d
{-# INLINE matrix #-}

viewspot :: V3 GLfloat -> Direction -> V3 GLfloat
viewspot position' direction =
    position' + (vista *^ vector direction)

vista :: GLfloat
vista = 10
