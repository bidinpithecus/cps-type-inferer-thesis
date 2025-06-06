isSubtypeOfPoly :: CPSPolyType -> CPSPolyType ->
   Either TypeError (Maybe Substitution)
isSubtypeOfPoly (Forall vars1 t1) (Forall vars2 t2) = 
  runTI $ do
    fv1 <- replicateM (length vars1) freshTVar
    let s1 = Map.fromList (zip vars1 fv1)
    fv2 <- replicateM (length vars2) freshTVar
    let s2 = Map.fromList (zip vars2 fv2)
    pure $ isSubtypeOf (applySubst s1 t1) (applySubst s2 t2)


isSubtypeOf :: CPSMonoType -> CPSMonoType -> Maybe Substitution
isSubtypeOf t1 t2 = match t1 t2 Map.empty
  where
    match :: CPSMonoType -> CPSMonoType -> Substitution ->
      Maybe Substitution
    match (TVar a) t subst =
        case Map.lookup a subst of
            Just tExisting -> 
                if tExisting == t
                    then Just subst
                    else Nothing
            Nothing ->
                if occursCheck a t
                    then Nothing
                    else Just (Map.insert a t subst)
    match TInt TInt subst = Just subst
    match (TNeg ts1) (TNeg ts2) subst
        | length ts1 == length ts2 = 
          foldM 
            (\s (t1', t2') -> match t1' t2' s)
            subst
            (zip ts1 ts2)
        | otherwise = Nothing
    match _ _ _ = Nothing
