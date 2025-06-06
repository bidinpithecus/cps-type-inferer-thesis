callByName :: Expr -> Id -> FreshM Command
callByName (Var x) k = return $ Jump x [k]

callByName (Lam x e) k = do
  k' <- freshCont
  v <- freshVar
  let jump = Jump k [v]
  eCall <- callByName e k'
  return $ Bind jump v [x, k'] eCall

callByName (App f e) k = do
  fCont <- freshCont
  fVar <- freshVar
  argCont <- freshCont
  argVar <- freshVar
  fCall <- callByName f fCont
  let appCmd = Jump fVar [argVar, k]
  eCall <- callByName e argCont
  let innerBind = Bind appCmd argVar [argCont] eCall
  return $ Bind fCall fCont [fVar] innerBind

callByName (Let x b a) k = do
  k' <- freshCont
  bCall <- callByName b k'
  aCall <- callByName a k
  return $ Bind aCall x [k'] bCall


cbnExprTrans :: Expr -> Command
cbnExprTrans expr = evalState (callByName expr initialCont) (0, 0)
