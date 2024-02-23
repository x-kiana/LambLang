typeCheck :: Expr -> Type -> Either Expr String

type TypeEnv = _

checkType :: TypeEnv -> Expr -> Type -> Either String Expr

synthType :: TypeEnv -> Expr -> Either String (Expr, Type)
