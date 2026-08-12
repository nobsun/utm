-- # Interactive
-- 
{-# LANGUAGE GHC2024 #-}
{-# LANGUAGE ImplicitParams #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE LexicalNegation #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE NPlusKPatterns #-}
{-# LANGUAGE DataKinds, PolyKinds, NoStarIsType, TypeFamilyDependencies #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedRecordDot, NoFieldSelectors, DuplicateRecordFields #-}

module Interactive
    ( Dio
    , interacts
    ) where

import Control.Monad.RWS
import System.IO

type Response = String
type Request  = String

interacts :: ([Response] -> [Request]) -> IO ()
interacts f = mapM_ ((>> hFlush stdout) . putStrLn) . f . lines =<< getContents

type Dio = RWS () [Request] [Response]

drive :: Dio a -> [Response] -> [Request]
drive act = snd . evalDio act

dio :: ([Response] -> [Request]) -> Dio ()
dio t = rws (\ _ s -> ((), [], t s))

evalDio :: Dio a -> [Response] -> (a,[Request])
evalDio = flip evalRWS ()

getResponse :: Dio Response
getResponse = get >>= (modify (drop 1) >>) . (pure . (!! 0))

putRequest :: Request -> Dio ()
putRequest req = tell [req]
