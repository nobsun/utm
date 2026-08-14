-- # Language.Turing.Wolfram_2_3
-- 
-- ## 言語拡張と`module`宣言
--
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
module Language.Turing.Wolfram_2_3
    where

import Data.Char
import Data.List
import Data.Map ( Map )
import Data.Map qualified as M
import Text.Printf
import Debug.Trace

import Language.Turing.TM
--
-- ## データタイプ
--
-- ### TM(2,3)のプログラム
--
program :: Program Q2 S3
program = (A, δ)
--
-- ### 状態 Q
--
data Q2
    = A
    | B
    deriving (Eq, Ord, Enum, Bounded, Show, Read)
--
-- ### 記号
--
data S3
    = O
    | I
    | Z
    deriving (Eq, Ord, Enum, Bounded, Show, Read) 
--
-- ### 遷移表 δ
--
δ :: Delta Q2 S3
δ = M.fromList
  [((A,O),(I,R,B)), ((B,O),(Z,L,A))
  ,((A,I),(Z,L,A)), ((B,I),(Z,R,B))
  ,((A,Z),(I,L,A)), ((B,Z),(O,R,A))
  ]
--
-- ### TM(2,3)
--
type TMQ2S3 = TM Q2 S3
--
-- ## 実行器
--
initTape :: Tape S3
initTape = Tape
    { toffset = 0
    , tlefts  = repeat O
    , thead   = O
    , trights = repeat O
    }

terminator :: TMQ2S3 -> Maybe String
terminator tm = if tm.tape.toffset >= dispLen
    then Just "head at display end"
    else Nothing

showCell :: S3 -> String
showCell = \ case
    O -> "\ESC[7m \ESC[0m"
    I -> "\ESC[43m \ESC[0m"
    Z -> "\ESC[41m \ESC[0m"
