{-# LANGUAGE RecordWildCards #-}

module Lib
    ( Defs(..), Model(..), Name, someFunc
    ) where

import qualified Data.List as List
import Text.Printf (printf)

type Name = String

data Defs = Defs
    { d_modelValues :: [Name]
    , d_constants :: [(Name, String)] }

data Model = Model
    { m_module :: Name
    , m_invariants :: [String]
    , m_properties :: [String ]
    , m_specification :: Name
    , m_constants :: Defs }

defs :: Defs
defs = Defs
    { d_modelValues = ["ClassA", "ClassB"]
    , d_constants = [("Classes", "{ ClassA, ClassB }"), ("Procs", "<<ClassA, ClassA, ClassB>>")]
    }

model :: Model
model = Model
    { m_module = "rwlock"
    , m_constants = defs
    , m_specification = "Spec"
    , m_invariants = ["TypeOk"]
    , m_properties = ["NoRace"]
    }

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

        constants = "CONSTANTS " ++ List.intercalate "," d_modelValues

        varPrefixF :: Name -> Int -> String
        varPrefixF name ix = printf "gen%d_%s" ix name

        addVar :: Int -> (String, String) -> (String, String)
        addVar ix (name, expr) = (name', printf "%s == %s" name' expr)
            where name' = varPrefixF name ix

        (names, defs') = unzip $ zipWith addVar [0..] d_constants
        remap = zip (map fst d_constants) names

        body = List.intercalate [""] [[header], [extends], [constants], defs', [footer]]

        mc = unlines body

        cfg = Cfg mcName model d_modelValues remap

genCfg :: Cfg -> String
genCfg Cfg {..} = unlines body
    where
        taggedEntry tag xs = tag : map ("    " ++) xs

        modelValues = taggedEntry "CONSTANTS" $ map (\x -> printf "%s = %s" x x) c_modelValues
        constants = taggedEntry "CONSTANT" $ map (uncurry $ printf "%s <- %s") c_remap
        spec = taggedEntry "SPECIFICATION " [m_specification c_model]
        invs = taggedEntry "INVARIANT" $ m_invariants c_model
        props = taggedEntry "PROPERTY" $ m_properties c_model
        body = List.intercalate [""] [modelValues, constants, spec, invs, props]

someFunc :: IO ()
someFunc = do
    let (mcBody, cfg) = genMC model
        cfgBody = genCfg cfg

    writeFile (c_mcName cfg ++ ".tla") mcBody
    writeFile (c_mcName cfg ++ ".cfg") cfgBody
