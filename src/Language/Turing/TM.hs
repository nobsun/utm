-- # Language.Turing.TM
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
module Language.Turing.TM
    where

import Data.Char
import Data.Maybe
import Data.List
import Data.Map ( Map )
import Data.Map qualified as M
import Text.Printf
--
-- ## データタイプ
--
-- ### プログラム
-- 
type Program q s = (q, Delta q s)
--
-- ### 移動方向
-- 
data D
    = L
    | R
    deriving (Eq, Ord, Enum, Bounded, Show, Read)
--
-- ### 遷移表 δ
--
type Delta q s = Map (q,s) (s,D,q)
--
-- ### Tape
-- 
type Offset = Int

data Tape s
    = Tape
    { toffset :: Offset
    , tlefts  :: [s]
    , thead   :: s
    , trights :: [s]
    }

write :: s -> Tape s -> Tape s
write s t = t { thead = s }

move :: D -> Tape s -> Tape s
move d t = case d of
    L -> case uncons t.tlefts of
        Nothing      -> error "move L: exceed left end"
        Just (h',ls) -> t { toffset = pred t.toffset
                          , tlefts  = ls
                          , thead   = h'
                          , trights = t.thead : t.trights
                          }
    R -> case uncons t.trights of
        Nothing      -> error "move L: exceed left end"
        Just (h',rs) -> t { toffset = succ t.toffset
                          , tlefts  = t.thead : t.tlefts
                          , thead   = h'
                          , trights = rs
                          }
--
-- ### TM
--
data TM q s
    = TM 
    { ctrl  :: [String]
    , fini  :: Maybe String
    , stats :: Stats
    , delta :: Delta q s
    , inner :: q
    , tape  :: Tape s
    }

data Stats
    = Stats
    { cntr :: Int
    , lbnd :: Int
    , rbnd :: Int
    }

initStats :: Stats
initStats = Stats
    { cntr = 0
    , lbnd = 0
    , rbnd = 0
    }
--
-- ## 実行器
--
run :: ( Ord q, Ord s
       , ?showCell   :: s -> String             -- 記号の印字表現
       , ?terminator :: TM q s -> Maybe String  -- 強制中断用
       )
    => Program q s  -- プログラム
    -> Tape s       -- 初期テープ
    -> [String]     -- 制御用入力
    -> [String]     -- トレース
run prog tape ctrl
    = showTrace
    $ eval
    $ setControl ctrl
    $ initTM prog tape
--
-- ### 初期化
--
setControl :: [String] -> (TM q s -> TM q s)
setControl ctrl utm = utm { ctrl = ctrl }

initTM :: Program q s -> Tape s -> TM q s
initTM (q0, δ) tape = TM 
    { ctrl  = []
    , fini  = Nothing
    , stats = initStats
    , delta = δ
    , inner = q0
    , tape  = tape
    }
--
-- ### Evaluator
--
eval :: (Ord q, Ord s, ?terminator :: TM q s -> Maybe String) 
     => TM q s -> [TM q s]
eval m = m : ms where
    ms | isFinal m = []
       | otherwise = eval m'
    m' = doAdmin (exec m)
--
-- ### 終了判定
--
isFinal :: TM q s -> Bool
isFinal tm = isJust tm.fini
--
-- ### 統計情報の更新
--
doAdmin :: TM q s -> TM q s
doAdmin tm = tm
    { stats = stats' }
    where
        stats' = tm.stats
            { cntr = succ tm.stats.cntr
            , lbnd = min tm.stats.lbnd tm.tape.toffset 
            , rbnd = max tm.stats.rbnd tm.tape.toffset
            }
--
-- ### ステップ実行
--
exec :: (Ord q, Ord s, ?terminator :: TM q s -> Maybe String)
     => TM q s -> TM q s
exec = control . proc

proc :: (Ord q, Ord s)
     => TM q s -> TM q s
proc tm = case tm.delta M.!? (tm.inner, tm.tape.thead) of
    Nothing      -> tm { fini = Just "no rule match" }
    Just (h,d,q) -> tm { inner = q
                       , tape  = move d (write h tm.tape)
                       }

control :: (?terminator :: TM q s -> Maybe String)
        => TM q s -> TM q s
control tm = case ?terminator tm of
    Just msg -> tm { fini = Just msg }
    Nothing  -> case map toLower $ tm.ctrl !! 0 of
        ""  -> tm { ctrl = drop 1 tm.ctrl }
        "c" -> tm { ctrl = repeat "" }
        s | all isDigit s -> tm { ctrl = replicate (read s) "" ++ drop 1 tm.ctrl }
          | otherwise     -> tm { ctrl = drop 1 tm.ctrl }
--
-- ### トレース表示器
--
showTrace :: (?showCell :: s -> String)
          => [TM q s] -> [String]
showTrace = map' (showTM, showStats)

map' :: (TM q s -> String, Stats -> String) -> [TM q s] -> [String]
map' (f,g) = \ case
    [tm]   -> [f tm ++ "\n" ++ g tm.stats ]
    tm:tms -> f tm : map' (f,g) tms
    _      -> []

showTM :: (?showCell :: s -> String)
       => TM q s -> String
showTM tm = concat
    [ concat [ printf "% 6d: " tm.stats.cntr
             , lefts'
             , ?showCell tp.thead
             , rights'
             ]
    , "\n"
    , showCursor curspos
    ]
    where
        curtape = tm.tape
        curspos = curtape.toffset
        tp      = resetpos curtape
        lefts'  = concatMap ?showCell $ reverse $ take dispLen tp.tlefts
        rights' = concatMap ?showCell $ take dispLen tp.trights

dispLen :: Int
dispLen = 60

showCursor :: Int -> String
showCursor pos = if pos < 0
    then replicate 8 ' ' ++ take barlen (drop (abs pos) bar)
    else replicate 8 ' ' ++ take barlen (drop (barlen - pos) bar)
    where
        bar = cycle $ replicate dispLen ' ' ++ "^" ++ replicate dispLen ' '
        barlen = 2 * dispLen + 1

resetpos :: Tape s -> Tape s
resetpos t = case compare 0 t.toffset of
    LT -> resetpos (move L t)
    EQ -> t
    GT -> resetpos (move R t)

showStats :: Stats -> String
showStats stats 
    = printf "steps: %d, left most %d, right most %d"
             stats.cntr
             stats.lbnd
             stats.rbnd
