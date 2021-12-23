{-# LANGUAGE QuasiQuotes #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Plugins.Print.Constraints (pprList, pprCts) where

import Language.Haskell.Printf (s)
import GHC.Corroborate

-- | Pretty print a list using leading commas with each element on its own line.
--
-- >>> putStrLn $ pprList [1..3]
-- [ 1
-- , 2
-- , 3
-- ]
pprList :: Show a => [a] -> String
pprList xs = go xs "" where
    go :: Show a => [a] -> ShowS
    go [] = showString ""
    go (y : ys) =
        showString "[ "
        . shows y
        . foldr
            (\e m -> showString "\n, " . shows e . m)
            (showString "\n]")
            ys

-- | Pretty print the constraints as a list of lists in the order of given,
-- derived and wanted.
pprCts
    :: [Ct] -- ^ Given constraints
    -> [Ct] -- ^ Derived constraints
    -> [Ct] -- ^ Wanted constraints
    -> [String]
pprCts gs ds ws =
    [ [s|>>> GHC-Givens = %s|] $ pprList gs
    , [s|>>> GHC-Derived = %s|] $ pprList ds
    , [s|>>> GHC-Wanteds = %s|] $ pprList ws
    ]

maybeExtractTyEq :: Ct -> Maybe ((Type, Type), Ct)
maybeExtractTyEq ct =
    case classifyPredType $ ctPred ct of
        EqPred NomEq t1 t2 -> return ((t1, t2), ct)
        _ -> Nothing

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

instance Show Ct where
    show ct = case maybeExtractTyEq ct of
        Just ((t1, t2),_) -> show (t1, t2)
        Nothing -> showSDocUnsafe $ ppr ct

instance Show WantedConstraints where
    show = showSDocUnsafe . ppr

instance Show EvTerm where
    show = showSDocUnsafe . ppr
