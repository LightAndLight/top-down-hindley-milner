module Type where

data Type
  = Var String
  | Arrow Type Type
  | Meta Int
  deriving (Eq, Show)

data Scheme = Scheme [String] Type
  deriving (Eq, Show)

display :: Type -> String
display ty =
  case ty of
    Var name ->
      name
    Arrow a b ->
      ( case a of
          Arrow{} -> parens
          _ -> id
      )
        (display a)
        <> " -> "
        <> display b
    Meta meta ->
      "?" <> show meta
 where
  parens a = "(" <> a <> ")"
