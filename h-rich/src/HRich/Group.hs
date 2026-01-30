{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ExistentialQuantification #-}
{-|
Module      : HRich.Group
Description : Group multiple renderables vertically.
Copyright   : (c) Ji-Haeng Huh, 2025
License     : BSD-3-Clause
Maintainer  : jhhuh.note@gmail.com

This module provides the 'Group' type for stacking multiple renderables
vertically, similar to Rich's Group class.
-}
module HRich.Group
    ( Group(..)
    , group
    ) where

import HRich.Renderable

-- | A group of renderables stacked vertically
data Group = forall a. Renderable a => Group [a]

-- | Create a group from a list of renderables
group :: Renderable a => [a] -> Group
group = Group

instance Renderable Group where
    render options (Group items) = concatMap (render options) items
    renderLines options (Group items) = concatMap (renderLines options) items
