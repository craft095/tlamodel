module Types ( Defs(..), Model(..), BindName(..), BoundValue(..), Name) where

type Name = String

data BindName = BindName Name [Name]
    deriving (Show, Eq)

data BoundValue
    = ModelValue
    | ModelValues { bv_symmetry :: Bool, bv_modelValues :: [Name] }
    | Expression String
    deriving (Show, Eq)

newtype Defs = Defs { d_constants :: [(BindName, BoundValue)] }
    deriving (Show, Eq)

data Model = Model
    { m_module :: Name
    , m_checkDeadlock :: Bool
    , m_invariants :: [String]
    , m_properties :: [String ]
    , m_specification :: Name
    , m_constants :: Defs }
    deriving (Show, Eq)
