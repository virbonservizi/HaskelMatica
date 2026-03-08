module Eval (eval, runEval, EvalError(..), Value(..), VarEnv, Number(..)) where

import AST
import Control.Monad (when, filterM)
import Control.Monad.State
import Control.Monad.Except
import qualified Data.Map.Strict as Map
import Data.List (intercalate, findIndex)

-- Tipos de Valores

data Value = VNum Number
           | VBool Bool
           | VSet [Number]
           | VMatrix [[Number]]
           | VVector [Number]
           | VSeq [Number]
           | VStr String
           deriving (Show, Eq)

-- Entornos

-- Entorno de variables
type VarEnv = Map.Map Variable Value

-- Entorno de funciones: nombre -> (parámetros, cuerpo, expresión de retorno)
type FuncEnv = Map.Map Variable ([Variable], Comm, Exp)

data Env = Env
    { varEnv  :: VarEnv   -- Variables
    , funcEnv :: FuncEnv  -- Funciones definidas
    }

initState :: Env
initState = Env
    { varEnv  = Map.empty
    , funcEnv = Map.empty
    }

-- Errores de evaluación

data EvalError 
    = DivByZero
    | VarNotFound Variable
    | FuncNotFound Variable
    | TypeMismatch String
    | IndexOutOfBounds String
    | InvalidOperation String
    | ArgumentMismatch Variable Int Int
    | EmptySet String
    | MatrixDimensionMismatch String
    | RuntimeError String
    deriving (Show, Eq)

-- Mostrar errores de forma amigable
showError :: EvalError -> String
showError DivByZero = "Error: División por cero"
showError (VarNotFound v) = "Error: Variable '" ++ v ++ "' no definida"
showError (FuncNotFound f) = "Error: Función '" ++ f ++ "' no definida"
showError (TypeMismatch msg) = "Error de tipo: " ++ msg
showError (IndexOutOfBounds msg) = "Error: Índice fuera de rango - " ++ msg
showError (InvalidOperation msg) = "Error: Operación inválida - " ++ msg
showError (ArgumentMismatch f expected got) = 
    "Error: Función '" ++ f ++ "' espera " ++ show expected ++ 
    " argumentos, pero recibió " ++ show got
showError (EmptySet msg) = "Error: Conjunto vacío - " ++ msg
showError (MatrixDimensionMismatch msg) = "Error de dimensiones: " ++ msg
showError (RuntimeError msg) = "Error de ejecución: " ++ msg

-- Mónada de evaluación

type EvalM a = StateT Env (ExceptT EvalError IO) a

runEval :: EvalM a -> IO (Either EvalError (a, Env))
runEval action = runExceptT (runStateT action initState)

eval :: Comm -> IO (Either String VarEnv)
eval program = do
    result <- runEval (evalComm program)
    case result of
        Left err -> return $ Left (showError err)
        Right (_, finalState) -> return $ Right (varEnv finalState)

-- Funciones auxiliares de estado

lookupVar :: Variable -> EvalM Value
lookupVar var = do
    env <- gets varEnv
    case Map.lookup var env of
        Nothing -> throwError (VarNotFound var)
        Just v  -> return v

updateVar :: Variable -> Value -> EvalM ()
updateVar var val = modify $ \s -> s { varEnv = Map.insert var val (varEnv s) }

registerFunc :: Variable -> [Variable] -> Comm -> Exp -> EvalM ()
registerFunc name params body ret = 
    modify $ \s -> s { funcEnv = Map.insert name (params, body, ret) (funcEnv s) }

lookupFunc :: Variable -> EvalM ([Variable], Comm, Exp)
lookupFunc fname = do
    fenv <- gets funcEnv
    case Map.lookup fname fenv of
        Nothing -> throwError (FuncNotFound fname)
        Just f  -> return f

-- Evaluación de Comandos

evalComm :: Comm -> EvalM ()
evalComm Skip = return ()

evalComm (Assign var expr) = do
    val <- evalExp expr
    updateVar var val

evalComm (SeqComm c1 c2) = do
    evalComm c1
    evalComm c2

evalComm (If cond thenBranch maybeElseBranch) = do
    condVal <- evalBoolExp cond
    if condVal
        then evalComm thenBranch
        else case maybeElseBranch of
            Just elseBranch -> evalComm elseBranch
            Nothing -> return ()

evalComm (While cond body) = do
    condVal <- evalBoolExp cond
    when condVal $ do
        evalComm body
        evalComm (While cond body)

evalComm (For init cond step body) = do
    evalComm init
    condVal <- evalBoolExp cond
    when condVal $ do
        evalComm body
        evalComm step
        evalComm (For Skip cond step body)

evalComm (ForEach var seqExp body) = do
    seqVal <- evalSeqExp seqExp
    oldVar <- gets (Map.lookup var . varEnv)
    case seqVal of
        [] -> return ()
        (x:xs) -> do
            updateVar var (VNum x)
            evalComm body
            evalComm (ForEach var (SeqLit (map Const xs)) body)
    case oldVar of
        Nothing -> modify $ \s -> s { varEnv = Map.delete var (varEnv s) }
        Just v  -> updateVar var v

evalComm (FuncDef name params body retExp) = do
    registerFunc name params body retExp

evalComm (Print exprs) = do
    vals <- mapM evalExpForPrint exprs
    liftIO $ putStrLn $ unwords vals
    where
        evalExpForPrint expr = do
            val <- evalExp expr
            return $ valueToString val

evalComm (Error expr) = do
    val <- evalExp expr
    throwError $ RuntimeError $ valueToString val

valueToString :: Value -> String
valueToString (VNum (I n)) = show n
valueToString (VNum (F f)) = show f
valueToString (VBool b) = show b
valueToString (VSet nums) = "{" ++ intercalate ", " (map showNum nums) ++ "}"
valueToString (VVector nums) = "(" ++ intercalate ", " (map showNum nums) ++ ")"
valueToString (VSeq nums) = "<" ++ intercalate ", " (map showNum nums) ++ ">"
valueToString (VMatrix rows) = "[" ++ intercalate "; " (map showRow rows) ++ "]"
    where showRow row = intercalate ", " (map showNum row)
valueToString (VStr s) = s

showNum :: Number -> String
showNum (I n) = show n
showNum (F f) = show f

-- Evaluación de Expresiones

evalExp :: Exp -> EvalM Value
evalExp (ENum e) = do
    num <- evalNumExp e
    return (VNum num)

evalExp (EBool e) = do
    b <- evalBoolExp e
    return (VBool b)

evalExp (ESet e) = do
    s <- evalSetExp e
    return (VSet s)

evalExp (EMatrix e) = do
    m <- evalMatrixExp e
    return (VMatrix m)

evalExp (EVector e) = do
    v <- evalVectorExp e
    return (VVector v)

evalExp (ESeq e) = do
    seq <- evalSeqExp e
    return (VSeq seq)

evalExp (EStr e) = do
    str <- evalStrExp e
    return (VStr str)

evalExp (EVar v) = lookupVar v

evalExp (FunCall fname args) = do
    (params, body, retExp) <- lookupFunc fname
    when (length params /= length args) $
        throwError $ ArgumentMismatch fname (length params) (length args)
    argVals <- mapM evalExp args
    oldEnv <- gets varEnv
    mapM_ (\(p, v) -> updateVar p v) (zip params argVals)
    evalComm body
    result <- evalExp retExp
    modify $ \s -> s { varEnv = oldEnv }
    return result

-- Evaluación de Expresiones Numéricas

evalNumExp :: NumExp -> EvalM Number
evalNumExp (Const n) = return n
evalNumExp (VarNum v) = do
    val <- lookupVar v
    case val of
        VNum n -> return n
        _ -> throwError $ TypeMismatch ("Variable '" ++ v ++ "' no es un número")

evalNumExp Pi = return $ F pi

evalNumExp E = return $ F (exp 1)

evalNumExp (Neg e) = do
    n <- evalNumExp e
    return $ negateNum n

evalNumExp (Add e1 e2) = do
    n1 <- evalNumExp e1
    n2 <- evalNumExp e2
    return $ addNum n1 n2

evalNumExp (Sub e1 e2) = do
    n1 <- evalNumExp e1
    n2 <- evalNumExp e2
    return $ subNum n1 n2

evalNumExp (Mul e1 e2) = do
    n1 <- evalNumExp e1
    n2 <- evalNumExp e2
    return $ mulNum n1 n2

evalNumExp (Div e1 e2) = do
    n1 <- evalNumExp e1
    n2 <- evalNumExp e2
    when (isZero n2) $ throwError DivByZero
    return $ divNum n1 n2

evalNumExp (Mod e1 e2) = do
    n1 <- evalNumExp e1
    n2 <- evalNumExp e2
    when (isZero n2) $ throwError DivByZero
    case (n1, n2) of
        (I a, I b) -> return $ I (a `mod` b)
        _ -> throwError $ TypeMismatch "Operación módulo solo válida para enteros"

evalNumExp (Pow e1 e2) = do
    n1 <- evalNumExp e1
    n2 <- evalNumExp e2
    return $ powNum n1 n2

evalNumExp (Abs e) = do
    n <- evalNumExp e
    return $ absNum n

evalNumExp (Sqrt e) = do
    n <- evalNumExp e
    return $ sqrtNum n

evalNumExp (Sin e) = do
    n <- evalNumExp e
    return $ F (sin (numToFloat n))

evalNumExp (Cos e) = do
    n <- evalNumExp e
    return $ F (cos (numToFloat n))

evalNumExp (Exp e) = do
    n <- evalNumExp e
    return $ F (exp (numToFloat n))

evalNumExp (Log base arg) = do
    b <- evalNumExp base
    a <- evalNumExp arg
    return $ F (logBase (numToFloat b) (numToFloat a))

evalNumExp (Cardinal setExp) = do
    set <- evalSetExp setExp
    return $ I (length set)

evalNumExp (SetMin setExp) = do
    set <- evalSetExp setExp
    when (null set) $ throwError $ EmptySet "min"
    return $ minimum set

evalNumExp (SetMax setExp) = do
    set <- evalSetExp setExp
    when (null set) $ throwError $ EmptySet "max"
    return $ maximum set

evalNumExp (SetSum setExp) = do
    set <- evalSetExp setExp
    return $ foldl addNum (I 0) set

evalNumExp (MatAccess matExp rowExp colExp) = do
    mat <- evalMatrixExp matExp
    row <- evalNumExp rowExp
    col <- evalNumExp colExp
    case (row, col) of
        (I r, I c) -> do
            when (r <= 0 || r > length mat) $
                throwError $ IndexOutOfBounds ("Fila " ++ show r ++ " fuera de rango")
            let selectedRow = mat !! (r - 1)
            when (c <= 0 || c > length selectedRow) $
                throwError $ IndexOutOfBounds ("Columna " ++ show c ++ " fuera de rango")
            return $ selectedRow !! (c - 1)
        _ -> throwError $ TypeMismatch "Los índices de matriz deben ser enteros"

evalNumExp (MatRows matExp) = do
    mat <- evalMatrixExp matExp
    return $ I (length mat)

evalNumExp (MatCols matExp) = do
    mat <- evalMatrixExp matExp
    if null mat
        then return $ I 0
        else return $ I (length (head mat))

evalNumExp (VecDot v1Exp v2Exp) = do
    v1 <- evalVectorExp v1Exp
    v2 <- evalVectorExp v2Exp
    when (length v1 /= length v2) $
        throwError $ InvalidOperation "Los vectores deben tener la misma longitud"
    return $ foldl addNum (I 0) (zipWith mulNum v1 v2)

evalNumExp (VecMax vecExp) = do
    vec <- evalVectorExp vecExp
    when (null vec) $ throwError $ EmptySet "vector vacío en VecMax"
    return $ maximum vec

evalNumExp (VecMin vecExp) = do
    vec <- evalVectorExp vecExp
    when (null vec) $ throwError $ EmptySet "vector vacío en VecMin"
    return $ minimum vec

evalNumExp (VecAccess vecExp idxExp) = do
    vec <- evalVectorExp vecExp
    idx <- evalNumExp idxExp
    case idx of
        I i -> do

            when (i <= 0 || i > length vec) $
                throwError $ IndexOutOfBounds ("Índice " ++ show i ++ " fuera de rango en vector")
            return $ vec !! (i - 1)
        _ -> throwError $ TypeMismatch "El índice de vector debe ser entero"

evalNumExp (VecNorm1 vecExp) = do
    vec <- evalVectorExp vecExp
    return $ foldl addNum (I 0) (map absNum vec)

evalNumExp (VecNorm2 vecExp) = do
    vec <- evalVectorExp vecExp
    let squares = map (\n -> mulNum n n) vec
        sum = foldl addNum (I 0) squares
    return $ sqrtNum sum

evalNumExp (VecNormInf vecExp) = do
    vec <- evalVectorExp vecExp
    when (null vec) $ throwError $ EmptySet "vector vacío en VecNormInf"
    return $ maximum (map absNum vec)

evalNumExp (SeqAccess seqExp idxExp) = do
    seq <- evalSeqExp seqExp
    idx <- evalNumExp idxExp
    case idx of
        I i -> do
            when (i < 0 || i >= length seq) $
                throwError $ IndexOutOfBounds ("Índice " ++ show i ++ " fuera de rango en secuencia")
            return $ seq !! i
        _ -> throwError $ TypeMismatch "El índice de secuencia debe ser entero"

evalNumExp (SeqLength seqExp) = do
    seq <- evalSeqExp seqExp
    return $ I (length seq)

evalNumExp (SeqFind seqExp elemExp) = do
    seq <- evalSeqExp seqExp
    elem <- evalNumExp elemExp
    case findIndex (== elem) seq of
        Just i -> return $ I i
        Nothing -> return $ I (-1)

evalNumExp (FunCallNum fname args) = do
    val <- evalExp (FunCall fname args)
    case val of
        VNum n -> return n
        _ -> throwError $ TypeMismatch "La función no retorna un número"

-- Evaluación de Expresiones Booleanas

evalBoolExp :: BoolExp -> EvalM Bool
evalBoolExp BTrue = return True

evalBoolExp BFalse = return False

evalBoolExp (VarBool v) = do
    val <- lookupVar v
    case val of
        VBool b -> return b
        _ -> throwError $ TypeMismatch ("Variable '" ++ v ++ "' no es booleana")

evalBoolExp (Eq e1 e2) = do
    n1 <- evalNumExp e1
    n2 <- evalNumExp e2
    return $ n1 == n2

evalBoolExp (Neq e1 e2) = do
    n1 <- evalNumExp e1
    n2 <- evalNumExp e2
    return $ n1 /= n2

evalBoolExp (Lt e1 e2) = do
    n1 <- evalNumExp e1
    n2 <- evalNumExp e2
    return $ n1 < n2

evalBoolExp (Lte e1 e2) = do
    n1 <- evalNumExp e1
    n2 <- evalNumExp e2
    return $ n1 <= n2

evalBoolExp (Gt e1 e2) = do
    n1 <- evalNumExp e1
    n2 <- evalNumExp e2
    return $ n1 > n2

evalBoolExp (Gte e1 e2) = do
    n1 <- evalNumExp e1
    n2 <- evalNumExp e2
    return $ n1 >= n2

evalBoolExp (And e1 e2) = do
    b1 <- evalBoolExp e1
    if not b1
        then return False
        else evalBoolExp e2

evalBoolExp (Or e1 e2) = do
    b1 <- evalBoolExp e1
    if b1
        then return True
        else evalBoolExp e2

evalBoolExp (Not e) = do
    b <- evalBoolExp e
    return $ not b

evalBoolExp (SetMember setExp numExp) = do
    set <- evalSetExp setExp
    num <- evalNumExp numExp
    return $ num `elem` set

evalBoolExp (SeqContains seqExp numExp) = do
    seq <- evalSeqExp seqExp
    num <- evalNumExp numExp
    return $ num `elem` seq

evalBoolExp (Forall var setExp boolExp) = do
    set <- evalSetExp setExp
    oldEnv <- gets varEnv
    results <- mapM (\n -> do
        updateVar var (VNum n)
        evalBoolExp boolExp) set
    modify $ \s -> s { varEnv = oldEnv }
    return $ and results

evalBoolExp (Exists var setExp boolExp) = do
    set <- evalSetExp setExp
    oldEnv <- gets varEnv
    results <- mapM (\n -> do
        updateVar var (VNum n)
        evalBoolExp boolExp) set
    modify $ \s -> s { varEnv = oldEnv }
    return $ or results

evalBoolExp (FunCallBool fname args) = do
    val <- evalExp (FunCall fname args)
    case val of
        VBool b -> return b
        _ -> throwError $ TypeMismatch "La función no retorna un booleano"

-- Evaluación de Conjuntos

evalSetExp :: SetExp -> EvalM [Number]
evalSetExp (SetLit exprsNum) = do 
    nums <- mapM evalNumExp exprsNum
    return (removeDuplicates nums)

evalSetExp (VarSet v) = do
    val <- lookupVar v
    case val of
        VSet s -> return s
        _ -> throwError $ TypeMismatch ("Variable '" ++ v ++ "' no es un conjunto")

evalSetExp (Range start end Nothing) = do
    s <- evalNumExp start
    e <- evalNumExp end
    case (s, e) of
        (I startInt, I endInt) -> return [I i | i <- [startInt .. endInt]]
        _ -> do
            -- Para flotantes o mixtos, convertir a flotantes con step de 1.0
            let startFloat = numToFloat s
                endFloat = numToFloat e
            return [F x | x <- takeWhile (<= endFloat) [startFloat, startFloat + 1.0 ..]]

evalSetExp (Range start end (Just step)) = do
    s <- evalNumExp start
    e <- evalNumExp end
    st <- evalNumExp step
    case (s, e, st) of
        (I startInt, I endInt, I stepInt) -> 
            return [I i | i <- [startInt, startInt + stepInt .. endInt]]
        _ -> do
            -- Convertir todo a flotantes para soportar rangos mixtos
            let startFloat = numToFloat s
                endFloat = numToFloat e
                stepFloat = numToFloat st
            when (stepFloat == 0) $ 
                throwError $ InvalidOperation "El paso del rango no puede ser cero"
            if stepFloat > 0
                then return [F x | x <- takeWhile (<= endFloat) [startFloat, startFloat + stepFloat ..]]
                else return [F x | x <- takeWhile (>= endFloat) [startFloat, startFloat + stepFloat ..]]
                
evalSetExp (Union s1 s2) = do
    set1 <- evalSetExp s1
    set2 <- evalSetExp s2
    return $ removeDuplicates (set1 ++ set2)

evalSetExp (Inter s1 s2) = do
    set1 <- evalSetExp s1
    set2 <- evalSetExp s2
    return $ [x | x <- set1, x `elem` set2]

evalSetExp (Diff s1 s2) = do
    set1 <- evalSetExp s1
    set2 <- evalSetExp s2
    return $ [x | x <- set1, x `notElem` set2]

evalSetExp (SymDiff s1 s2) = do
    set1 <- evalSetExp s1
    set2 <- evalSetExp s2
    let inS1NotS2 = [x | x <- set1, x `notElem` set2]
        inS2NotS1 = [x | x <- set2, x `notElem` set1]
    return $ removeDuplicates (inS1NotS2 ++ inS2NotS1)

evalSetExp (SetCompr var (Gen genVar genSet) conditions) = do
    when (var /= genVar) $
        throwError $ InvalidOperation "Variable de comprensión debe coincidir con el generador"
    baseSet <- evalSetExp genSet
    oldEnv <- gets varEnv
    filtered <- filterM (\n -> do
        updateVar var (VNum n)
        results <- mapM evalBoolExp conditions
        return $ and results) baseSet
    modify $ \s -> s { varEnv = oldEnv }
    return filtered

evalSetExp (Insert varName numExp) = do
    val <- lookupVar varName
    case val of
        VSet s -> do
            num <- evalNumExp numExp
            return $ removeDuplicates (s ++ [num])
        _ -> throwError $ TypeMismatch ("Variable '" ++ varName ++ "' no es un conjunto")

evalSetExp (Remove varName numExp) = do
    val <- lookupVar varName
    case val of
        VSet s -> do
            num <- evalNumExp numExp
            return $ filter (/= num) s
        _ -> throwError $ TypeMismatch ("Variable '" ++ varName ++ "' no es un conjunto")

evalSetExp (FunCallSet fname args) = do
    val <- evalExp (FunCall fname args)
    case val of
        VSet s -> return s
        _ -> throwError $ TypeMismatch "La función no retorna un conjunto"

-- Evaluación de Matrices

evalMatrixExp :: MatrixExp -> EvalM [[Number]]
evalMatrixExp (MatLit rows) = do
    evaluatedRows <- mapM (mapM evalNumExp) rows
    when (not (null evaluatedRows) && not (allSameLength evaluatedRows)) $
        throwError $ MatrixDimensionMismatch "Todas las filas de la matriz deben tener la misma longitud"
    return evaluatedRows
    where
        allSameLength [] = True
        allSameLength [_] = True
        allSameLength (x:y:xs) = length x == length y && allSameLength (y:xs)

evalMatrixExp (VarMat v) = do
    val <- lookupVar v
    case val of
        VMatrix m -> return m
        _ -> throwError $ TypeMismatch ("Variable '" ++ v ++ "' no es una matriz")

evalMatrixExp (MatZeros rowsExp colsExp) = do
    rows <- evalNumExp rowsExp
    cols <- evalNumExp colsExp
    case (rows, cols) of
        (I r, I c) -> do
            when (r <= 0 || c <= 0) $
                throwError $ InvalidOperation "Las dimensiones deben ser positivas"
            return $ replicate r (replicate c (I 0))
        _ -> throwError $ TypeMismatch "Las dimensiones de matriz deben ser enteros"

evalMatrixExp (MatOnes rowsExp colsExp) = do
    rows <- evalNumExp rowsExp
    cols <- evalNumExp colsExp
    case (rows, cols) of
        (I r, I c) -> do
            when (r <= 0 || c <= 0) $
                throwError $ InvalidOperation "Las dimensiones deben ser positivas"
            return $ replicate r (replicate c (I 1))
        _ -> throwError $ TypeMismatch "Las dimensiones de matriz deben ser enteros"

evalMatrixExp (MatEye sizeExp) = do
    size <- evalNumExp sizeExp
    case size of
        I n -> do
            when (n <= 0) $
                throwError $ InvalidOperation "El tamaño debe ser positivo"
            return [[if i == j then I 1 else I 0 | j <- [0..n-1]] | i <- [0..n-1]]
        _ -> throwError $ TypeMismatch "El tamaño de matriz debe ser entero"

evalMatrixExp (MatSlice matExp r1Exp r2Exp c1Exp c2Exp) = do
    mat <- evalMatrixExp matExp
    r1 <- evalNumExp r1Exp
    r2 <- evalNumExp r2Exp
    c1 <- evalNumExp c1Exp
    c2 <- evalNumExp c2Exp
    case (r1, r2, c1, c2) of
        (I rStart, I rEnd, I cStart, I cEnd) -> do
            when (rStart < 0 || rEnd >= length mat || rStart > rEnd) $
                throwError $ IndexOutOfBounds "Índices de fila inválidos"
            when (null mat) $
                throwError $ InvalidOperation "Matriz vacía"
            let numCols = length (head mat)
            when (cStart < 0 || cEnd >= numCols || cStart > cEnd) $
                throwError $ IndexOutOfBounds "Índices de columna inválidos"
            let selectedRows = take (rEnd - rStart + 1) $ drop rStart mat
                sliced = map (\row -> take (cEnd - cStart + 1) $ drop cStart row) selectedRows
            return sliced
        _ -> throwError $ TypeMismatch "Los índices deben ser enteros"

evalMatrixExp (MatAdd m1Exp m2Exp) = do
    m1 <- evalMatrixExp m1Exp
    m2 <- evalMatrixExp m2Exp
    when (length m1 /= length m2) $
        throwError $ MatrixDimensionMismatch "Las matrices deben tener el mismo número de filas"
    when (null m1 || null m2) $
        throwError $ InvalidOperation "Matriz vacía"
    when (length (head m1) /= length (head m2)) $
        throwError $ MatrixDimensionMismatch "Las matrices deben tener el mismo número de columnas"
    return $ zipWith (zipWith addNum) m1 m2

evalMatrixExp (MatSub m1Exp m2Exp) = do
    m1 <- evalMatrixExp m1Exp
    m2 <- evalMatrixExp m2Exp
    when (length m1 /= length m2) $
        throwError $ MatrixDimensionMismatch "Las matrices deben tener el mismo número de filas"
    when (null m1 || null m2) $
        throwError $ InvalidOperation "Matriz vacía"
    when (length (head m1) /= length (head m2)) $
        throwError $ MatrixDimensionMismatch "Las matrices deben tener el mismo número de columnas"
    return $ zipWith (zipWith subNum) m1 m2

evalMatrixExp (MatMul m1Exp m2Exp) = do
    m1 <- evalMatrixExp m1Exp
    m2 <- evalMatrixExp m2Exp
    when (null m1 || null m2) $
        throwError $ InvalidOperation "Matriz vacía"
    let cols1 = length (head m1)
        rows2 = length m2
    when (cols1 /= rows2) $
        throwError $ MatrixDimensionMismatch 
            ("Incompatible para multiplicación: " ++ show cols1 ++ " != " ++ show rows2)
    let cols2 = length (head m2)
        result = [[foldl addNum (I 0) [mulNum (m1 !! i !! k) (m2 !! k !! j) | k <- [0..cols1-1]]
                                                                            | j <- [0..cols2-1]]
                                                                            | i <- [0..length m1-1]]
    return result

evalMatrixExp (MatTrans matExp) = do
    mat <- evalMatrixExp matExp
    if null mat
        then return []
        else if null (head mat)
            then return []
            else do
                let numCols = length (head mat)
                return [[row !! j | row <- mat] | j <- [0..numCols-1]]

evalMatrixExp (MatDiag matExp) = do
    mat <- evalMatrixExp matExp
    when (null mat) $
        throwError $ InvalidOperation "Matriz vacía"
    let rows = length mat
        cols = length (head mat)
    when (rows /= cols) $
        throwError $ MatrixDimensionMismatch "La operación diagonal requiere matriz cuadrada"
    return [[if i == j then mat !! i !! j else I 0 | j <- [0..cols-1]] | i <- [0..rows-1]]

evalMatrixExp (MatTriU matExp) = do
    mat <- evalMatrixExp matExp
    when (null mat) $
        throwError $ InvalidOperation "Matriz vacía"
    let rows = length mat
        cols = length (head mat)
    when (rows /= cols) $
        throwError $ MatrixDimensionMismatch "La operación triangular superior requiere matriz cuadrada"
    return [[if i <= j then mat !! i !! j else I 0 | j <- [0..cols-1]] | i <- [0..rows-1]]

evalMatrixExp (MatTriL matExp) = do
    mat <- evalMatrixExp matExp
    when (null mat) $
        throwError $ InvalidOperation "Matriz vacía"
    let rows = length mat
        cols = length (head mat)
    when (rows /= cols) $
        throwError $ MatrixDimensionMismatch "La operación triangular inferior requiere matriz cuadrada"
    return [[if i >= j then mat !! i !! j else I 0 | j <- [0..cols-1]] | i <- [0..rows-1]]

evalMatrixExp (MatSwapRows matExp r1Exp r2Exp) = do
    mat <- evalMatrixExp matExp
    r1 <- evalNumExp r1Exp
    r2 <- evalNumExp r2Exp
    case (r1, r2) of
        (I row1, I row2) -> do
            when (row1 <= 0 || row1 > length mat) $
                throwError $ IndexOutOfBounds ("Fila " ++ show row1 ++ " fuera de rango")
            when (row2 <= 0 || row2 > length mat) $
                throwError $ IndexOutOfBounds ("Fila " ++ show row2 ++ " fuera de rango")
            let swapped = [if i == row1 then mat !! (row2 - 1)
                          else if i == row2 then mat !! (row1 - 1)
                          else mat !! (i - 1)
                          | i <- [1..length mat]]
            return swapped
        _ -> throwError $ TypeMismatch "Los índices de fila deben ser enteros"

evalMatrixExp (MatSwapCols matExp c1Exp c2Exp) = do
    mat <- evalMatrixExp matExp
    c1 <- evalNumExp c1Exp
    c2 <- evalNumExp c2Exp
    when (null mat) $
        throwError $ InvalidOperation "Matriz vacía"
    case (c1, c2) of
        (I col1, I col2) -> do
            let numCols = length (head mat)
            when (col1 <= 0 || col1 > numCols) $
                throwError $ IndexOutOfBounds ("Columna " ++ show col1 ++ " fuera de rango")
            when (col2 <= 0 || col2 > numCols) $
                throwError $ IndexOutOfBounds ("Columna " ++ show col2 ++ " fuera de rango")
            let swapped = [map (\j -> if j == col1 then row !! (col2 - 1)
                                      else if j == col2 then row !! (col1 - 1)
                                      else row !! (j - 1))
                          [1..numCols]
                          | row <- mat]
            return swapped
        _ -> throwError $ TypeMismatch "Los índices de columna deben ser enteros"

evalMatrixExp (FunCallMat fname args) = do
    val <- evalExp (FunCall fname args)
    case val of
        VMatrix m -> return m
        _ -> throwError $ TypeMismatch "La función no retorna una matriz"

-- Evaluación de Vectores

evalVectorExp :: VectorExp -> EvalM [Number]
evalVectorExp (VecLit exprs) = mapM evalNumExp exprs

evalVectorExp (VarVec v) = do
    val <- lookupVar v
    case val of
        VVector vec -> return vec
        _ -> throwError $ TypeMismatch ("Variable '" ++ v ++ "' no es un vector")

evalVectorExp (VecZeros sizeExp) = do
    size <- evalNumExp sizeExp
    case size of
        I n -> do
            when (n <= 0) $
                throwError $ InvalidOperation "El tamaño debe ser positivo"
            return $ replicate n (I 0)
        _ -> throwError $ TypeMismatch "El tamaño del vector debe ser entero"

evalVectorExp (VecSlice vecExp startExp endExp) = do
    vec <- evalVectorExp vecExp
    start <- evalNumExp startExp
    end <- evalNumExp endExp
    case (start, end) of
        (I s, I e) -> do
            when (s < 0 || e >= length vec || s > e) $
                throwError $ IndexOutOfBounds "Índices de slice inválidos"
            return $ take (e - s + 1) $ drop s vec
        _ -> throwError $ TypeMismatch "Los índices deben ser enteros"

evalVectorExp (VecAdd v1Exp v2Exp) = do
    v1 <- evalVectorExp v1Exp
    v2 <- evalVectorExp v2Exp
    when (length v1 /= length v2) $
        throwError $ InvalidOperation "Los vectores deben tener la misma longitud"
    return $ zipWith addNum v1 v2

evalVectorExp (VecSub v1Exp v2Exp) = do
    v1 <- evalVectorExp v1Exp
    v2 <- evalVectorExp v2Exp
    when (length v1 /= length v2) $
        throwError $ InvalidOperation "Los vectores deben tener la misma longitud"
    return $ zipWith subNum v1 v2

evalVectorExp (VecScale scalarExp vecExp) = do
    scalar <- evalNumExp scalarExp
    vec <- evalVectorExp vecExp
    return $ map (mulNum scalar) vec

evalVectorExp (MatRow matExp rowExp) = do
    mat <- evalMatrixExp matExp
    row <- evalNumExp rowExp
    when (null mat) $
        throwError $ InvalidOperation "Matriz vacía"
    case row of
        I r -> do
            when (r < 0 || r >= length mat) $
                throwError $ IndexOutOfBounds ("Fila " ++ show r ++ " fuera de rango")
            return $ mat !! r
        _ -> throwError $ TypeMismatch "El índice de fila debe ser entero"

evalVectorExp (MatCol matExp colExp) = do
    mat <- evalMatrixExp matExp
    col <- evalNumExp colExp
    when (null mat) $
        throwError $ InvalidOperation "Matriz vacía"
    case col of
        I c -> do
            let numCols = length (head mat)
            when (c < 0 || c >= numCols) $
                throwError $ IndexOutOfBounds ("Columna " ++ show c ++ " fuera de rango")
            return [row !! c | row <- mat]
        _ -> throwError $ TypeMismatch "El índice de columna debe ser entero"

evalVectorExp (FunCallVec fname args) = do
    val <- evalExp (FunCall fname args)
    case val of
        VVector v -> return v
        _ -> throwError $ TypeMismatch "La función no retorna un vector"

evalVectorExp (VecMatMul vecExp matExp) = do
    vec <- evalVectorExp vecExp
    mat <- evalMatrixExp matExp
    
    when (null mat) $
        throwError $ InvalidOperation "Matriz vacía"
    
    let numRows = length mat
    let numCols = if null mat then 0 else length (head mat)
    
    when (length vec /= numRows) $
        throwError $ MatrixDimensionMismatch 
            ("Vector de longitud " ++ show (length vec) ++ 
             " no puede multiplicarse con matriz de " ++ show numRows ++ " filas")
    return [foldl addNum (I 0) [mulNum (vec !! i) ((mat !! i) !! j) | i <- [0..numRows-1]] | j <- [0..numCols-1]]

evalVectorExp (MatVecMul matExp vecExp) = do
    mat <- evalMatrixExp matExp
    vec <- evalVectorExp vecExp
    
    when (null mat) $
        throwError $ InvalidOperation "Matriz vacía"
    
    let numRows = length mat
    let numCols = if null mat then 0 else length (head mat)
    
    when (length vec /= numCols) $
        throwError $ MatrixDimensionMismatch 
            ("Matriz de " ++ show numCols ++ " columnas no puede multiplicarse con vector de longitud " ++ show (length vec))
    return [foldl addNum (I 0) [mulNum ((mat !! i) !! j) (vec !! j) | j <- [0..numCols-1]] | i <- [0..numRows-1]]

-- Evaluación de Secuencias

evalSeqExp :: SeqExp -> EvalM [Number]
evalSeqExp (SeqLit exprs) = mapM evalNumExp exprs

evalSeqExp (VarSeq v) = do
    val <- lookupVar v
    case val of
        VSeq seq -> return seq
        _ -> throwError $ TypeMismatch ("Variable '" ++ v ++ "' no es una secuencia")

evalSeqExp (SeqSlice seqExp startExp endExp) = do
    seq <- evalSeqExp seqExp
    start <- evalNumExp startExp
    end <- evalNumExp endExp
    case (start, end) of
        (I s, I e) -> do
            when (s < 0 || e >= length seq || s > e) $
                throwError $ IndexOutOfBounds "Índices de slice inválidos"
            return $ take (e - s + 1) $ drop s seq
        _ -> throwError $ TypeMismatch "Los índices deben ser enteros"

evalSeqExp (SeqConcat s1Exp s2Exp) = do
    s1 <- evalSeqExp s1Exp
    s2 <- evalSeqExp s2Exp
    return $ s1 ++ s2

evalSeqExp (SeqAppend seqExp numExp) = do
    seq <- evalSeqExp seqExp
    num <- evalNumExp numExp
    return $ seq ++ [num]

evalSeqExp (SeqPrepend seqExp numExp) = do
    seq <- evalSeqExp seqExp
    num <- evalNumExp numExp
    return $ num : seq

evalSeqExp (SeqFilter var seqExp boolExp) = do
    seq <- evalSeqExp seqExp
    oldEnv <- gets varEnv
    filtered <- filterM (\n -> do
        updateVar var (VNum n)
        evalBoolExp boolExp) seq
    modify $ \s -> s { varEnv = oldEnv }
    return filtered

evalSeqExp (FunCallSeq fname args) = do
    val <- evalExp (FunCall fname args)
    case val of
        VSeq s -> return s
        _ -> throwError $ TypeMismatch "La función no retorna una secuencia"

-- Evaluación de Strings

evalStrExp :: StrExp -> EvalM String
evalStrExp (StrLit s) = return s

evalStrExp (VarStr v) = do
    val <- lookupVar v
    case val of
        VStr s -> return s
        _ -> throwError $ TypeMismatch ("Variable '" ++ v ++ "' no es un string")

evalStrExp (FunCallStr fname args) = do
    val <- evalExp (FunCall fname args)
    case val of
        VStr s -> return s
        _ -> throwError $ TypeMismatch "La función no retorna un string"

-- Funciones auxiliares numéricas

numToFloat :: Number -> Float
numToFloat (I n) = fromIntegral n
numToFloat (F f) = f

isZero :: Number -> Bool
isZero (I 0) = True
isZero (F f) = f == 0.0
isZero _ = False

negateNum :: Number -> Number
negateNum (I n) = I (-n)
negateNum (F f) = F (-f)

addNum :: Number -> Number -> Number
addNum (I a) (I b) = I (a + b)
addNum a b = F (numToFloat a + numToFloat b)

subNum :: Number -> Number -> Number
subNum (I a) (I b) = I (a - b)
subNum a b = F (numToFloat a - numToFloat b)

mulNum :: Number -> Number -> Number
mulNum (I a) (I b) = I (a * b)
mulNum a b = F (numToFloat a * numToFloat b)

divNum :: Number -> Number -> Number
divNum (I a) (I b) = if a `mod` b == 0 
                     then I (a `div` b)
                     else F (fromIntegral a / fromIntegral b)
divNum a b = F (numToFloat a / numToFloat b)

powNum :: Number -> Number -> Number
powNum (I a) (I b) | b >= 0 = I (a ^ b)
powNum a b = F (numToFloat a ** numToFloat b)

absNum :: Number -> Number
absNum (I n) = I (abs n)
absNum (F f) = F (abs f)

sqrtNum :: Number -> Number
sqrtNum n = F (sqrt (numToFloat n))

removeDuplicates :: Eq a => [a] -> [a]
removeDuplicates [] = []
removeDuplicates (x:xs) = x : removeDuplicates (filter (/= x) xs)
