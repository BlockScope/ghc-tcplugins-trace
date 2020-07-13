{-# LANGUAGE QuasiQuotes #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Plugins.Print.Constraints
    ( TraceCallCount(..), TraceCts(..)
    , showList, pprSolverCallCount, pprCts
    ) where

import Prelude hiding (showList)
import Language.Haskell.Printf (s)
import Data.List (intercalate)
import GHC.Corroborate

newtype TraceCallCount = TraceCallCount Bool
newtype TraceCts = TraceCts Bool

pprSolverCallCount :: TraceCallCount -> Int -> String
pprSolverCallCount (TraceCallCount callCount) n
    | callCount = [s|>>> GHC-TcPlugin #%d|] n
    | otherwise = ""

pprCts
    :: [Ct] -- ^ Given constraints
    -> [Ct] -- ^ Derived constraints
    -> [Ct] -- ^ Wanted constraints
    -> [String]
pprCts gs ds ws =
    [ [s|>>> GHC-Givens = %s|] $ showList gs
    , [s|>>> GHC-Derived = %s|] $ showList ds
    , [s|>>> GHC-Wanteds = %s|] $ showList ws
    ]

maybeExtractTyEq :: Ct -> Maybe ((Type, Type), Ct)
maybeExtractTyEq ct =
    case classifyPredType $ ctPred ct of
        EqPred NomEq t1 t2 -> return ((t1, t2), ct)
        _ -> Nothing

-- *  Printing
instance Show Type where
    show ty = case splitTyConApp_maybe ty of
        Just (tcon, tys) -> show tcon ++ " " ++ show tys
        Nothing -> case getTyVar_maybe ty of
            Just var -> show var
            Nothing -> showSDocUnsafe $ ppr ty

instance Show TyCon where
    show = occNameString . getOccName

instance Show Var where
    show v =
        let nicename = varOccName v ++ ";" ++ varUnique v in
        nicename ++ ":" ++ classifyVar v

varOccName :: Var -> String
varOccName = showSDocUnsafe . ppr . getOccName

varUnique :: Var -> String
varUnique = show . getUnique

classifyVar :: Var -> String
classifyVar v
    | isTcTyVar v = if isMetaTyVar v then "t" else "s"
    | otherwise = "irr"

showList :: Show a => [a] -> String
showList [] = "[]"
showList xs = "[\n" ++ intercalate "\n" (show <$> xs) ++ "\n]"

instance Show Ct where
    show ct = case maybeExtractTyEq ct of
        Just ((t1, t2),_) -> show (t1, t2)
        Nothing -> showSDocUnsafe $ ppr ct

instance Show WantedConstraints where
    show = showSDocUnsafe . ppr

instance Show EvTerm where
    show = showSDocUnsafe . ppr
