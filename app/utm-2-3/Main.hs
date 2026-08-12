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
import Language.Turing.Wolfram_2_3

main :: IO ()
main = interact (unlines . usage . run initTape . lines)

usage :: [String] -> [String]
usage = (msg :) where
    msg = "\ESC[2J\ESC[0;0HUTM(2,3): press only <Enter> for a step, 'C' key + <Enter> for all steps"
