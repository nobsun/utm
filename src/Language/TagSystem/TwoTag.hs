-- # Language.TagSystem.TwoTag
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
module Language.TagSystem.TwoTag
    where

import Data.Char
import Data.Maybe
import Data.List
import Data.Map ( Map )
import Data.Map qualified as M
import Data.Sequence qualified as Q
import Data.Sequence.Internal qualified as Q
import Text.Printf
import Debug.Trace

--
-- ## データタイプ
--
-- ### タグシステム
-- 
type Program = (Str, Rule)
--
-- ### アルファベット
--
data Alphabet
    = A
    | B
    | C
    | H
    deriving (Eq, Ord, Enum, Bounded, Show, Read)
--
-- 文字列
-- 
type Str = Q.Seq Alphabet
--
-- 生成規則
--
type Rule = Map Alphabet Str

collatz :: Program
collatz = (aaa, rule) where
    rule = M.fromList [(A,bc),(B,a),(C,aaa)]
    bc   = Q.fromList [B,C]
    a    = Q.fromList [A]
    aaa  = Q.fromList [A,A,A]
--
-- ### 2-Tag system
--
data TagSys
    = TagSys
    { ctrl  :: [String]
    , fini  :: Maybe String
    , rule  :: Rule
    , stats :: Stats
    , word  :: Str
    }

initTagSys :: Program -> TagSys
initTagSys (s,r) = TagSys
                 { ctrl  = []
                 , fini  = Nothing
                 , rule  = r
                 , stats = initStats
                 , word  = s
                 }

data Stats
    = Stats
    { cntr :: Int
    }

initStats :: Stats
initStats = Stats
    { cntr = 0
    }
--
-- ## 実行器
--
run :: Program -> [String] -> [String]
run prog ctrl
    = showTrace
    $ eval
    $ setControl ctrl
    $ initTagSys prog
--
-- ### 初期化
--
setControl :: [String] -> (TagSys -> TagSys)
setControl ctrl utm = utm { ctrl = ctrl }
--
-- ### Evaluator
--
eval :: TagSys -> [TagSys]
eval m = m : ms where
    ms | isFinal m = []
       | otherwise = eval m'
    m' = doAdmin (exec m)
--
-- ### 終了判定
--
isFinal :: TagSys -> Bool
isFinal tag = isJust tag.fini
--
-- ### 統計情報の更新
--
doAdmin :: TagSys -> TagSys
doAdmin tag = if isFinal tag then tag
              else tag { stats = stats' }
    where
        stats' = tag.stats
            { cntr = succ tag.stats.cntr
            }
--
-- ### ステップ実行
--
exec :: TagSys -> TagSys
exec = control . proc

proc :: TagSys -> TagSys
proc tag = case Q.viewl tag.word of
    Q.EmptyL -> tag { fini = Just "string empty" }
    s Q.:< _ -> if Q.length tag.word < 2 then tag { fini = Just "halt condition" }
        else case tag.rule M.!? s of
            Nothing  -> tag { fini = Just "no rule match" }
            Just str -> tag { word = Q.drop 2 (tag.word <> str) }

control :: TagSys -> TagSys
control tag = case map toLower $ tag.ctrl !! 0 of
    ""  -> tag { ctrl = drop 1 tag.ctrl }
    "c" -> tag { ctrl = repeat "" }
    s | all isDigit s -> tag { ctrl = replicate (read s) "" ++ drop 1 tag.ctrl }
      | otherwise     -> tag { ctrl = drop 1 tag.ctrl }
--
-- ### トレース表示器
--
showTrace :: [TagSys] -> [String]
showTrace = map showTag

showTag :: TagSys -> String
showTag tag = if isFinal tag
    then "Stopped! : " ++ fromJust (tag.fini) ++ "\n" ++ showStats tag.stats
    else concat [ concat [ printf "% 6d: " tag.stats.cntr
                         , concatMap showCell (foldr (:) [] tag.word )
                         ]
                ]

dispLen :: Int
dispLen = 60

showCell :: Alphabet -> String
showCell s = conv s
    where
        conv = \ case
            A -> "a"
            B -> "b"
            C -> "c"
            H -> "H"

showStats :: Stats -> String
showStats stats 
    = printf "steps: %d" stats.cntr
