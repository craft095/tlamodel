module Types ( Defs(..), Model(..), BindName(..), Name) where

type Name = String

data BindName = BindName Name [Name]

data Defs = Defs
    { d_modelValues :: [Name]
    , d_constants :: [(BindName, String)] }

data Model = Model
    { m_module :: Name
    , m_checkDeadlock :: Bool
    , m_invariants :: [String]
    , m_properties :: [String ]
    , m_specification :: Name
    , m_constants :: Defs }
