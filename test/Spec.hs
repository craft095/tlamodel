{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}

import Data.FileEmbed
import Data.String (IsString)
import Data.Text (Text)
import Test.HUnit
import Test.HUnit.Text
import Text.Megaparsec as P

import Types
import Parser
import Gen

tplFile0 :: Text
tplFile0 = $(embedStringFile "test/PriorityQueue.tpl")
tlaFile0 :: String
tlaFile0 = $(embedStringFile "test/MCPriorityQueue.tla")
cfgFile0 :: String
cfgFile0 = $(embedStringFile "test/MCPriorityQueue.cfg")

systemTests :: [Test]
systemTests = let
    Right model = parseAndResolveModel "PriorityQueue.tpl" tplFile0
    (mcBody, cfg) = genMC model
    cfgBody = genCfg cfg
    in
        [ TestCase $ assertEqual "Compare MC*.tla" mcBody tlaFile0
        , TestCase $ assertEqual "Compare MC*.cfg" cfgBody cfgFile0
        ]

parserCaseOk :: (Eq a, Show a) => String -> Parser a -> a -> Text -> Test
parserCaseOk title p expected content =
    TestCase (assertEqual title (Right expected) (P.runParser p "buffer" content))

case0 :: Test
case0 = parserCaseOk "identifier" identifierP expected content
    where
        content = "someId123!u"
        expected = "someId123!u"

case1 :: Test
case1 = parserCaseOk "Bind A Model Value" bindP expected content
    where
        content = "Procs <- [model value] X1;"
        expected = (BindName "Procs" [], ModelValue "X1")

case1_1 :: Test
case1_1 = parserCaseOk "Bind A Model Value/<-" bindP expected content
    where
        content = "Procs <- [model value] X1 Y <- Qwerty;"
        expected = (BindName "Procs" [], ModelValue "X1")

case2 :: Test
case2 = parserCaseOk "Bind A Model Values" bindP expected content
    where
        content = "Procs <- [model value] { X1, X2 };"
        expected = (BindName "Procs" [], ModelValues False ["X1", "X2"])

case2_1 :: Test
case2_1 = parserCaseOk "Bind A Model Values/<-" bindP expected content
    where
        content = "Procs <- [model value] { X1, X2 } Y <- Qwerty;"
        expected = (BindName "Procs" [], ModelValues False ["X1", "X2"])

case3 :: Test
case3 = parserCaseOk "Bind A Symmetrical Model Values" bindP expected content
    where
        content = "Procs <- [model value] <symmetrical> { X1, X2 };"
        expected = (BindName "Procs" [], ModelValues True ["X1", "X2"])

case3_1 :: Test
case3_1 = parserCaseOk "Bind A Symmetrical Model Values/<-" bindP expected content
    where
        content = "Procs <- [model value] <symmetrical> { X1, X2 } Y <- Qwerty;"
        expected = (BindName "Procs" [], ModelValues True ["X1", "X2"])

case4 :: Test
case4 = parserCaseOk "Bind An Scalar Expression" bindP expected content
    where
        content = "Procs <- [ a \\in {1..2} |-> a * a ];"
        expected = (BindName "Procs" [], Expression "[ a \\in {1..2} |-> a * a ]")

case4_1 :: Test
case4_1 = parserCaseOk "Bind An Scalar Expression/<-" bindP expected content
  where
    content = "Procs <- [ a \\in {1..2} |-> a * a ] X <- Y"
    expected = (BindName "Procs" [], Expression "[ a \\in {1..2} |-> a * a ] ")

tests :: Test
tests = TestList $
    [ case0
    , case1
    , case1_1
    , case2
    , case2_1
    , case3
    , case3_1
    , case4
    , case4_1
    ]
    ++ systemTests

main :: IO ()
main = runTestTTAndExit tests