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

--
-- ## データタイプ
--
-- ### プログラム
-- 
type Program = (Q, Delta)
--
-- ### 状態 Q
--
data Q
    = A
    | B
    deriving (Eq, Ord, Enum, Bounded, Show, Read)
--
-- ### 記号
--
data S
    = O
    | I
    | Z
    deriving (Eq, Ord, Enum, Bounded, Show, Read) 
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
type Delta = Map (Q,S) (Q,S,D)

δ :: Delta
δ = M.fromList
  [((A,O),(B,I,R)), ((B,O),(A,Z,L))
  ,((A,I),(A,Z,L)), ((B,I),(B,Z,R))
  ,((A,Z),(A,I,L)), ((B,Z),(A,O,R))]
--
-- ### Tape
-- 
type Offset = Int
data Tape 
    = Tape
    { toffset :: Offset
    , tlefts  :: [S]
    , thead   :: S
    , trights :: [S]
    }

write :: S -> Tape -> Tape
write s t = t { thead = s }

move :: D -> Tape -> Tape
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
-- ### UTM
--
data UTM
    = UTM 
    { ctrl  :: [String]
    , stats :: Stats
    , inner :: Q
    , tape  :: Tape
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
run :: Tape -> [String] -> [String]
run tape ctrl
    = showTrace
    $ eval
    $ setControl ctrl
    $ initUTM tape
--
-- ### 初期化
--
setControl :: [String] -> (UTM -> UTM)
setControl ctrl utm = utm { ctrl = ctrl }

initUTM :: Tape -> UTM
initUTM tape = UTM { ctrl  = [], stats = initStats
                   , inner = A, tape = tape
                   }
initTape :: Tape
initTape = Tape
    { toffset = 0
    , tlefts  = repeat O
    , thead   = O
    , trights = repeat O
    }
--
-- ### Evaluator
--
eval :: UTM -> [UTM]
eval m = m : ms where
    ms | isFinal m = []
       | otherwise = eval m'
    m' = doAdmin (exec m)
--
-- ### 終了判定
--
isFinal :: UTM -> Bool
isFinal utm = abs utm.tape.toffset >= dispLen
--
-- ### 統計情報の更新
--
doAdmin :: UTM -> UTM
doAdmin utm = utm
    { stats = stats' }
    where
        stats' = utm.stats
            { cntr = succ utm.stats.cntr
            , lbnd = min utm.stats.lbnd utm.tape.toffset 
            , rbnd = max utm.stats.rbnd utm.tape.toffset
            }
--
-- ### ステップ実行
--
exec :: UTM -> UTM
exec utm = case map toLower $ utm.ctrl !! 0 of
    ""  -> utm' { ctrl = drop 1 utm.ctrl }
    "c" -> utm' { ctrl = repeat "" }
    s | all isDigit s -> utm' { ctrl = replicate (read s) "" ++ drop 1 utm.ctrl }
      | otherwise     -> utm' { ctrl = drop 1 utm.ctrl }
    where
        utm' = case δ M.!? (q,h) of
            Nothing        -> error "exec: no rule match"
            Just (q',h',d) -> utm { inner = q'
                                  , tape = move d (write h' utm.tape) 
                                  }
            where
                q = utm.inner
                h = utm.tape.thead
--
-- ### トレース表示器
--
showTrace :: [UTM] -> [String]
showTrace = map showUTM

showUTM :: UTM -> String
showUTM utm = concat
    -- [ showStats utm.stats 
    -- , "\n"
    -- ] ++
    [ concat [ printf "% 6d: " utm.stats.cntr
             , lefts'
             , showCell tp.thead
             , rights'
             ]
    , "\n"
    , showCursor curspos
    ]
    where
        curtape = utm.tape
        curspos = curtape.toffset
        tp      = resetpos curtape
        lefts'  = concatMap showCell $ reverse $ take dispLen tp.tlefts
        rights' = concatMap showCell $ take dispLen tp.trights

dispLen :: Int
dispLen = 60

showCursor :: Int -> String
showCursor pos = if pos < 0
    then replicate 8 ' ' ++ take barlen (drop (abs pos) bar)
    else replicate 8 ' ' ++ take barlen (drop (barlen - pos) bar)
    where
        bar = cycle $ replicate dispLen ' ' ++ "^" ++ replicate dispLen ' '
        barlen = 2 * dispLen + 1

resetpos :: Tape -> Tape
resetpos t = case compare 0 t.toffset of
    LT -> resetpos (move L t)
    EQ -> t
    GT -> resetpos (move R t)

showCell :: S -> String
showCell s = conv s ++ " \ESC[0m"
    where
        conv = \ case
            O -> "\ESC[7m"
            I -> "\ESC[43m"
            Z -> "\ESC[41m"

showStats :: Stats -> String
showStats stats 
    = printf "steps: % 3d, left most % 3d, right most % 3d"
             stats.cntr
             stats.lbnd
             stats.rbnd
