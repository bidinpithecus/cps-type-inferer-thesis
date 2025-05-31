type Id = String

data Command
  = Jump Id [Id]
  | Bind Command Id [Id] Command

data CPSMonoType
  = TVar Id
  | TInt
  | TNeg [CPSMonoType]

data CPSPolyType
  = Forall [Id] CPSMonoType

type Context = Data.Map Id CPSPolyType
type Substitution = Data.Map Id CPSMonoType