{-# LANGUAGE CPP #-}
{-# LANGUAGE GHC2024 #-}
{-# LANGUAGE ImplicitParams #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE LexicalNegation #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE NPlusKPatterns #-}
{-# LANGUAGE DataKinds, PolyKinds, NoStarIsType, TypeFamilyDependencies #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedRecordDot, NoFieldSelectors, DuplicateRecordFields #-}
module Main
    ( main
    ) where

import Interact
import Language.TagSystem.TwoTag

main :: IO ()
main = interact (unlines . usage . run collatz . lines)

usage :: [String] -> [String]
usage = (msg :) where
    msg = "\ESC[2J\ESC[0;0H2 Tag System: press only <Enter> for a step, 'C' key + <Enter> for all steps"
