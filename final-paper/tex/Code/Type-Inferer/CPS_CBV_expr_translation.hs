callByValue :: Expr -> Id -> FreshM Command
callByValue (Var x) k = return $ Jump k [x]

callByValue (Lam x e) k = do
  k' <- freshCont
  v <- freshVar
  let bindBody = Jump k [v]
  body <- callByValue e k'
  return $ Bind bindBody v [x, k'] body

callByValue (App f e) k = do
  fCont <- freshCont
  fVar <- freshVar
  argCont <- freshCont
  argVar <- freshVar
  fCall <- callByValue f fCont
  eCall <- callByValue e argCont
  let appCmd = Jump fVar [argVar, k]
  let argBind = Bind eCall argCont [argVar] appCmd
  return $ Bind fCall fCont [fVar] argBind

callByValue (Let x b a) k = do
  k' <- freshCont
  bCall <- callByValue b k'
  aCall <- callByValue a k
  return $ Bind bCall k' [x] aCall


cbvExprTrans :: Expr -> Command
cbvExprTrans expr = evalState (callByValue expr initialCont) (0, 0)
