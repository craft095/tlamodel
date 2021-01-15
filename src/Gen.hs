{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE NamedFieldPuns #-}

module Gen (gen) where

import Types (Defs (..), Model (..), BindName(..), BoundValue(..), Name)
import Text.Printf (printf)

import qualified Data.List as List

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
        extends = "EXTENDS " ++ m_module model

        commaSep = List.intercalate ","

        varPrefixF :: Name -> Int -> String
        varPrefixF name ix = printf "gen%d_%s" ix name

        modelValues :: [Name]
        modelValues = concatMap (f . snd) d_constants
            where
                f (ModelValue n) = [n]
                f (ModelValues _ ns) = ns
                f _ = []

        symmetry :: [Name]
        symmetry = concatMap f d_constants
            where
                f (BindName n _, ModelValues True _) = [n]
                f _ = []

        prettyRhs :: BoundValue -> String
        prettyRhs (ModelValue name) = name
        prettyRhs (ModelValues _ names) = printf "{ %s }" $ commaSep names
        prettyRhs (Expression expr) = expr

        addVar :: Int -> (BindName, BoundValue) -> (String, String)
        addVar ix (lhs@(BindName name _), rhs) = (varPrefixF name ix, def)
            where
                var = varPrefixF name ix
                argsF (BindName _ []) = ""
                argsF (BindName _ args) = printf "(%s)" (commaSep args)
                def = printf "%s%s == %s" var (argsF lhs) (prettyRhs rhs)

        addSymmVar :: Int -> Name -> (String, String)
        addSymmVar ix name = (symmVar, symmDef)
            where
                var = varPrefixF name ix
                symmVar = "symm_" ++ var
                symmDef = printf "%s == Permutations(%s)" symmVar var

        (names, defs') = unzip $ zipWith addVar [0..] d_constants
        getName :: (BindName, a) -> Name
        getName (BindName name _, _) = name
        remap = zip (map getName d_constants) names

        (symmVars, symmDefs) = unzip $ zipWith addSymmVar [0..] symmetry

        constants =
            if null modelValues
            then ""
            else "CONSTANTS " ++ commaSep modelValues

        body = List.intercalate [""] [[header], [extends], [constants], defs', symmDefs, [footer]]

        mc = unlines body

        cfg = Cfg mcName model modelValues symmVars remap

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
