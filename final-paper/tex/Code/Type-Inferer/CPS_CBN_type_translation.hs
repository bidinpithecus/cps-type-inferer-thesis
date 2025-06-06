cbnTypeTranslation :: LambdaMonoType -> CPSPolyType
cbnTypeTranslation st =
  let (transMono, freeVars) = runState (cbnTrans st) S.empty
  in Forall (S.toList freeVars) transMono
  where
    cbnTrans :: LambdaMonoType -> State (S.Set String) CPSMonoType
    cbnTrans = \case
      Lambda.Typing.TVar varId -> do
        modify (S.insert varId)
        return $ TNeg [CPS.Typing.TVar varId]
      TArr a b -> do
        a' <- cbnTrans a
        b' <- cbnTrans b
        return $ TNeg [TNeg [TNeg [a'], b']]
