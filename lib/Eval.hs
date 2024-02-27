module Eval where
import DataTypes

-- Datatype representing values in LambLang
data Val =
     StrV String
   | UnitV
   | FunV (Val -> Either String Val)
   | IOV (IO (Either String Val))

-- defining equality, mostly for testing purposes
instance Eq Val where
    StrV x == StrV y = x == y
    UnitV == UnitV = True
    FunV x == FunV y = False
    IOV x == IOV y = False

-- defining show, for testing purposes
instance Show Val where
    show (StrV s) = show s
    show UnitV = show ()
    show (FunV x) = "<procedure>"
    show (IOV x) = "<IO>"

-- An environment to keep track of values
-- Implemented as a list of tuples of the variable names and their values
type ValEnv = [(String, Val)]

-- Return value associated with given variable name in given environment
-- Or error if it doesn't exist in the environment (i.e. unbound)
envLookup :: ValEnv -> String -> Either String Val
envLookup [] str = Left "Unbound variable"
envLookup ((x, y):rest) str
   | str == x = Right y
   | otherwise = envLookup rest str

-- Return new environment with given variable name and its value added to the given env
envPut :: String -> Val -> ValEnv -> ValEnv
envPut str v valEnv = (str, v):valEnv

-- builtin function value to print strings
printStr :: Val
printStr = FunV (\(StrV s) -> Right (IOV (putStr s >> return (Right UnitV))))

-- builtin IO value to read strings
readStr :: Val
readStr = IOV (do
  s <- readLn
  return (Right (StrV s)))

-- initial environment, includes the builting readStr and printStr values
initEnv :: ValEnv
initEnv = envPut "readStr" readStr (envPut "printStr" printStr [])

-- Evaluates the given LambLang expression in the initEnv
-- Assumes that Expr has type IOT UnitT, otherwise throws a runtime error
eval :: Expr -> IO ()
eval e =
    case eval' initEnv e of
      Right (IOV x) -> do
          y <- x
          (case y of
              Right UnitV -> return ()
              _ -> print "Expected unit value")
      Right _ -> do (print "Expected an IO")
      Left msg -> do
          print msg

-- Helper evaluater function called by the wrapper eval function
-- Evaluate given Expr in given environment, if evaluates to a value return it, otherwise throw a runtime error
eval' :: ValEnv -> Expr -> Either String Val
eval' valEnv (Var x) = envLookup valEnv x
eval' valEnv (Lam x body) = Right (FunV (\val -> (eval' (envPut x val valEnv) body)))
eval' valEnv (App fun arg) =
    case (eval' valEnv fun) of
        Right (FunV f) -> 
            case eval' valEnv arg of
                Right x -> f x
                Left x -> Left x
        Right _ -> Left "Expected a function value"
        Left msg -> Left msg
eval' valEnv (StrLit s) = Right (StrV s)
eval' valEnv UnitLit = Right UnitV
eval' valEnv (Ann expr t) = eval' valEnv expr
eval' valEnv (IOBind expr1 expr2) =
    do
    v1 <- eval' valEnv expr1
    v2 <- eval' valEnv expr2
    case (v1, v2) of
        (IOV x, FunV f) ->
            Right (IOV (do
                x' <- x
                case x' of
                    Right v ->
                        case f v of
                            Right (IOV p) -> p
                            Right _ -> return (Left "Expected an IO")
                            Left msg -> return (Left msg)
                    Left msg -> return (Left msg)))
        (_, FunV f) -> Left "Expected the first argument to IOBind to be a IO Value"
        (_, _) -> Left "Expected the second argument to IOBind to be a FunV"
eval' valEnv (IOReturn expr) = Right (IOV (return (eval' valEnv expr)))
