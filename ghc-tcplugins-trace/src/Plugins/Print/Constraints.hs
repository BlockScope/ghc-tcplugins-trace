{-# LANGUAGE DerivingStrategies, GeneralizedNewtypeDeriving #-}

{-# OPTIONS_GHC -Wno-orphans #-}

module Plugins.Print.Constraints (Indent(..), pprList, pprCts) where

import GHC.Corroborate

newtype Indent = Indent Int deriving newtype Num

-- | Pretty print a list using leading commas with each element on its own line.
--
-- >>> putStrLn $ pprList [1..3]
-- [ 1
-- , 2
-- , 3
-- ]
pprList :: Show a => Indent -> [a] -> ShowS
pprList (Indent i) xs = go xs where
    tab = replicate (2 * i) ' '

    go :: Show a => [a] -> ShowS
    go [] = showString "[]"
    go [y] = showString "[ " . shows y . showString " ]"
    go (y : ys) =
        showString "[ "
        . showString "\n"
        . showString tab
        . showString "  "
        . shows y
        . foldr
            (\e m -> showString "\n" . showString tab . showString ", " . shows e . m)
            (showString "\n" . showString tab . showString "]")
            ys

-- | Pretty print the constraints as a list of lists in the order of given,
-- derived and wanted.
pprCts
    :: Indent
    -> [Ct] -- ^ Given constraints
    -> [Ct] -- ^ Derived constraints
    -> [Ct] -- ^ Wanted constraints
    -> [String]
pprCts indent@(Indent i) gCts dCts wCts =
    [
        ( tab
        . showString "[constraints]"
        . showString "\n"
        . tabtab
        . showString "given = "
        . pprList j gCts
        . showString "\n"
        . tabtab
        . showString "derived = "
        . pprList j dCts
        . showString "\n"
        . tabtab
        . showString "wanted = "
        . pprList j wCts)
        ""
    ]
    where
        tab = showString $ replicate (2 * i) ' '
        tabtab = showString $ replicate (2 * (i + 1)) ' '
        j = indent + 1

maybeExtractTyEq :: Ct -> Maybe ((Type, Type), Ct)
maybeExtractTyEq ct =
    case classifyPredType $ ctPred ct of
        EqPred NomEq t1 t2 -> return ((t1, t2), ct)
        _ -> Nothing

instance Show Type where
    show ty = case splitTyConApp_maybe ty of
        Just (tcon, tys) ->
            ( shows tcon
            . showString " "
            . shows tys)
            ""
        Nothing -> case getTyVar_maybe ty of
            Just var -> show var
            Nothing -> showSDocUnsafe $ ppr ty

instance Show TyCon where
    show = occNameString . getOccName

instance Show Var where
    show v =
        ( showString (varOccName v)
        . showString ";"
        . showString (varUnique v)
        . showString ":"
        . showString (classifyVar v))
        ""

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
