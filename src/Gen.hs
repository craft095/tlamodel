{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE NamedFieldPuns #-}

module Gen where

import qualified Data.List as List
import Data.Maybe (catMaybes)
import Text.Printf (printf)

import Types (Defs (..), Model (..), BindName(..), BoundValue(..), Name)

data Cfg = Cfg
    { c_mcName :: Name
    , c_model :: Model
    , c_modelValues :: [Name]
    , c_symmetry :: [Name]
    , c_remap :: [(Name, Name)]
    }

genMC :: Model -> (String, Cfg)
genMC model = (mc, cfg)
    where
        Defs { d_constants } = m_constants model
        dashes = replicate 20 '-'
        mcName = "MC" ++ m_module model
        header = dashes ++ printf " MODULE %s " mcName ++ dashes
        footer = replicate (length header) '='
        extends = "EXTENDS TLC, " ++ m_module model

        commaSep = List.intercalate ","

        varPrefixF :: Name -> Int -> String
        varPrefixF name ix = printf "gen%d_%s" ix name

        -- New model values come from sets: SomeSet <- [model value] { NewModelValue0, NewModelValue1}
        newModelValues :: [Name]
        newModelValues = concatMap (f . snd) d_constants
            where
                f (ModelValues _ ns) = ns
                f _ = []

        -- All model values
        allModelValues :: [Name]
        allModelValues = concatMap f d_constants
            where
                f (BindName n _, ModelValue) = [n]
                f (_, ModelValues _ ns) = ns
                f _ = []

        symmetry :: [Name]
        symmetry = concatMap f d_constants
            where
                f (BindName n _, ModelValues True _) = [n]
                f _ = []

        prettyRhs :: BoundValue -> String
        prettyRhs ModelValue = ""
        prettyRhs (ModelValues _ names) = printf "{ %s }" $ commaSep names
        prettyRhs (Expression expr) = expr

        addVar :: Int -> (BindName, BoundValue) -> (String, String, Maybe String)
        addVar ix (lhs@(BindName name _), rhs) = (varPrefixF name ix, def, symmVar)
            where
                var = varPrefixF name ix
                argsF (BindName _ []) = ""
                argsF (BindName _ args) = printf "(%s)" (commaSep args)
                def = printf "%s%s == %s" var (argsF lhs) (prettyRhs rhs)
                symmVar = case rhs of
                    ModelValues True _ -> Just ("symm_" ++ var)
                    _ -> Nothing

        addSymmVar :: Name -> Maybe Name -> Maybe (String, String)
        addSymmVar _ Nothing = Nothing
        addSymmVar var (Just symmVar) = Just (symmVar, symmDef)
            where
                symmDef = printf "%s == Permutations(%s)" symmVar var

        -- Remove all single model values
        constants' = filter (isNotModelValue . snd) d_constants
            where
                isNotModelValue ModelValue = False
                isNotModelValue _ = True

        (names, defs', symmNames) = unzip3 $ zipWith addVar [0..] constants'
        getName :: (BindName, a) -> Name
        getName (BindName name _, _) = name
        remap = zip (map getName constants') names

        (symmVars, symmDefs) = unzip . catMaybes $ zipWith addSymmVar names symmNames

        constants =
            if null newModelValues
            then ""
            else "CONSTANTS " ++ commaSep newModelValues

        body = List.intercalate [""] [[header], [extends], [constants], defs', symmDefs, [footer]]

        mc = unlines body
        cfg = Cfg mcName model allModelValues symmVars remap

genCfg :: Cfg -> String
genCfg Cfg {..} = unlines body
    where
        taggedEntry _ [] = []
        taggedEntry tag xs = tag : map ("    " ++) xs

        modelValues = taggedEntry "CONSTANTS" $ map (\x -> printf "%s = %s" x x) c_modelValues
        constants = taggedEntry "CONSTANT" $ map (uncurry $ printf "%s <- %s") c_remap
        symmetry = taggedEntry "SYMMETRY" c_symmetry
        spec = taggedEntry "SPECIFICATION " [m_specification c_model]
        deadlock = taggedEntry "CHECK_DEADLOCK " [if m_checkDeadlock c_model then "TRUE" else "FALSE"]
        invs = taggedEntry "INVARIANT" $ m_invariants c_model
        props = taggedEntry "PROPERTY" $ m_properties c_model
        body = List.intercalate [""] [modelValues, constants, symmetry, spec, deadlock, invs, props]

gen :: Model -> IO ()
gen model = do
    let (mcBody, cfg) = genMC model
        cfgBody = genCfg cfg

    writeFile (c_mcName cfg ++ ".tla") mcBody
    writeFile (c_mcName cfg ++ ".cfg") cfgBody
