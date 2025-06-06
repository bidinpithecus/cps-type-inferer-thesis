cbvTypeTranslation :: LambdaMonoType -> CPSPolyType
cbvTypeTranslation st =
  let (transMono, freeVars) = runState (cbvTrans st) S.empty
      finalMono = TNeg [transMono]
  in Forall (S.toList freeVars) finalMono
  where
    cbvTrans :: LambdaMonoType -> State (S.Set String) CPSMonoType
    cbvTrans = \case
      Lambda.Typing.TVar varId -> do
        modify (S.insert varId)
        return $ CPS.Typing.TVar varId
      TArr a b -> do
        a' <- cbvTrans a
        b' <- cbvTrans b
        return $ TNeg [a', TNeg [b']]
