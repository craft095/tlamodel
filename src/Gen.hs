{-# LANGUAGE RecordWildCards #-}

module Gen (gen) where

import Types (Defs (..), Model (..), BindName(..), Name)
import Text.Printf (printf)

import qualified Data.List as List

data Cfg = Cfg
    { c_mcName :: Name
    , c_model :: Model
    , c_modelValues :: [Name]
    , c_remap :: [(Name, Name)]
    }

genMC :: Model -> (String, Cfg)
genMC model = (mc, cfg)
    where
        Defs {..} = m_constants model
        dashes = replicate 20 '-'
        mcName = "MC" ++ m_module model
        header = dashes ++ printf " MODULE %s " mcName ++ dashes
        footer = replicate (length header) '='
        extends = "EXTENDS " ++ m_module model

        constants =
            if null d_modelValues
            then ""
            else "CONSTANTS " ++ List.intercalate "," d_modelValues

        varPrefixF :: Name -> Int -> String
        varPrefixF name ix = printf "gen%d_%s" ix name

        addVar :: Int -> (BindName, String) -> (String, String)
        addVar ix (BindName name args, expr) = (name', printf "%s%s == %s" name' (argsF args) expr)
            where
                name' = varPrefixF name ix
                argsF [] = ""
                argsF xs = printf "(%s)" $ List.intercalate "," xs

        (names, defs') = unzip $ zipWith addVar [0..] d_constants
        getName :: (BindName, a) -> Name
        getName (BindName name _, _) = name
        remap = zip (map getName d_constants) names

        body = List.intercalate [""] [[header], [extends], [constants], defs', [footer]]

        mc = unlines body

        cfg = Cfg mcName model d_modelValues remap

genCfg :: Cfg -> String
genCfg Cfg {..} = unlines body
    where
        taggedEntry _ [] = []
        taggedEntry tag xs = tag : map ("    " ++) xs

        modelValues = taggedEntry "CONSTANTS" $ map (\x -> printf "%s = %s" x x) c_modelValues
        constants = taggedEntry "CONSTANT" $ map (uncurry $ printf "%s <- %s") c_remap
        spec = taggedEntry "SPECIFICATION " [m_specification c_model]
        deadlock = taggedEntry "CHECK_DEADLOCK " [if m_checkDeadlock c_model then "TRUE" else "FALSE"]
        invs = taggedEntry "INVARIANT" $ m_invariants c_model
        props = taggedEntry "PROPERTY" $ m_properties c_model
        body = List.intercalate [""] [modelValues, constants, spec, deadlock, invs, props]

gen :: Model -> IO ()
gen model = do
    let (mcBody, cfg) = genMC model
        cfgBody = genCfg cfg

    writeFile (c_mcName cfg ++ ".tla") mcBody
    writeFile (c_mcName cfg ++ ".cfg") cfgBody
