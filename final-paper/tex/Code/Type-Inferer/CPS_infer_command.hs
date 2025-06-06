inferAtom :: Context -> Id -> TI CPSMonoType
inferAtom ctx x =
  case readMaybe x :: Maybe Integer of
    Just _  -> return TInt
    Nothing -> case Map.lookup x ctx of
                 Just poly -> do
                   t <- instantiate poly
                   return t
                 Nothing -> throwError $ UnboundVariable x ctx


inferCommand :: Context -> Command -> TI Substitution
inferCommand ctx (Jump k xs) = do
  t1 <- inferAtom ctx k
  t2 <- mapM (inferAtom ctx) xs
  s <- mgu t1 (TNeg t2)
  return s

inferCommand ctx (Bind b y ys c) = do
  paramTypes <- mapM (const freshTVar) ys
  let ctxParams = extendContextWithParams ctx ys paramTypes
  s1 <- inferCommand ctxParams c
  let contType = applySubst s1 (TNeg paramTypes)
  let ctxSubst = applySubstToContext s1 ctx
  let sigma = generalize ctxSubst contType
  let ctx' = Map.insert y sigma ctxSubst
  s2 <- inferCommand ctx' b
  return (composeSubst s2 s1)


inferWithCtx :: Command -> TI CPSPolyType
inferWithCtx cmd = do
  initialType <- freshTVar
  let ctx = Map.singleton initialCont (Forall [] initialType)

  subst <- inferCommand ctx cmd
  let ctx' = applySubstToContext subst ctx

  case Map.lookup initialCont ctx' of
    Just (Forall _ monoType) ->
      let gen = generalize (Map.delete initialCont ctx') monoType
          normalized = normalizePolyType gen
      in return normalized
    Nothing -> throwError (UnboundVariable initialCont ctx)
