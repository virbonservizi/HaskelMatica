module Parser where

import Text.ParserCombinators.Parsec
import Text.Parsec.Token
import Text.Parsec.Language (emptyDef)
import AST

totParser :: Parser a -> Parser a
totParser p = do
                  whiteSpace mathLang
                  t <- p
                  eof
                  return t

-- Analizador de Tokens
mathLang :: TokenParser u
mathLang = makeTokenParser (emptyDef   
    { commentStart    = "/*"
    , commentEnd      = "*/"
    , commentLine     = "//"
    , reservedNames   = ["true", "false", "skip", "if", "then", "else", 
                         "end", "while", "do", "for", "foreach", "in",
                         "pi", "e", "abs", "sqrt", "sin", "cos", "exp", "log",
                         "cardinal", "min", "max", "sum",
                         "rows", "cols", "prod", "scal", "norm",
                         "length", "find", "zeros", "ones", "eye", "trans",
                         "diag", "triu", "tril", "swaprows", "swapcols",
                         "union", "intersection", "difference", "symmetricdiff",
                         "insert", "remove", "contains",
                         "filter", "forall", "exists", "print", "error", "mod",
                         "concat", "append", "prepend", "slice", "access", "def", "return"]
    , reservedOpNames = ["+", "-", "*", "/", "^", "=", "!=", "<", "<=", 
                         ">", ">=", "&&", "||", "!", "|", "..", ":=",
                         ":", ",", ";", ".", "<-",
                         ".*", ".+", ".-", ".#*",
                         "#+", "#-", "#*", "#.*"]
    })

-- Parser de expresiones numéricas

numexp :: Parser NumExp
numexp = chainl1 term addopp

term = chainl1 powTerm multopp

powTerm = chainl1 factor powopp

factor = try (parens mathLang numexp)
     <|> try (do reservedOp mathLang "-"
                 f <- factor
                 return (Neg f))
     <|> try parseNumFunctions
     <|> try parseNumConstants
     <|> try parseSetOperations
     <|> try parseMatrixOperations
     <|> try parseVectorOperations
     <|> try parseSeqOperations
     <|> try parseNumber
     <|> try parseFunCallNum
     <|> try parseVarOrAccess

powopp = do try (reservedOp mathLang "^")
            return Pow

multopp = do try (reservedOp mathLang "*")
             return Mul
          <|> do try (reservedOp mathLang "/")
                 return Div
          <|> do try (reserved mathLang "mod")
                 return Mod

addopp = do try (reservedOp mathLang "+")
            return Add
         <|> do try (reservedOp mathLang "-")
                return Sub


parseNumFunctions :: Parser NumExp
parseNumFunctions = 
        try (do reserved mathLang "abs"
                e <- parens mathLang numexp
                return (Abs e))
    <|> try (do reserved mathLang "sqrt"
                e <- parens mathLang numexp
                return (Sqrt e))
    <|> try (do reserved mathLang "sin"
                e <- parens mathLang numexp
                return (Sin e))
    <|> try (do reserved mathLang "cos"
                e <- parens mathLang numexp
                return (Cos e))
    <|> try (do reserved mathLang "exp"
                e <- parens mathLang numexp
                return (Exp e))
    <|> try parseLog

parseLog :: Parser NumExp
parseLog = do
    reserved mathLang "log"
    symbol mathLang "("
    base <- numexp
    comma mathLang
    val <- numexp
    symbol mathLang ")"
    return (Log base val)


parseNumConstants :: Parser NumExp
parseNumConstants = 
        (do reserved mathLang "pi"
            return Pi)
    <|> (do reserved mathLang "e"
            return E)


parseSetOperations :: Parser NumExp
parseSetOperations = 
        try (do reserved mathLang "cardinal"
                s <- parens mathLang setexp
                return (Cardinal s))
    <|> try (do reserved mathLang "min"
                s <- parens mathLang setexp
                return (SetMin s))
    <|> try (do reserved mathLang "max"
                s <- parens mathLang setexp
                return (SetMax s))
    <|> try (do reserved mathLang "sum"
                s <- parens mathLang setexp
                return (SetSum s))

parseMatrixOperations :: Parser NumExp
parseMatrixOperations = 
        try (do reserved mathLang "rows"
                m <- parens mathLang matrixexp
                return (MatRows m))
    <|> try (do reserved mathLang "cols"
                m <- parens mathLang matrixexp
                return (MatCols m))

parseVectorOperations :: Parser NumExp
parseVectorOperations = 
        try parseProd
    <|> try parseNorm
    <|> try (do reserved mathLang "max"
                v <- parens mathLang vectorexp
                return (VecMax v))
    <|> try (do reserved mathLang "min"
                v <- parens mathLang vectorexp
                return (VecMin v))

parseProd :: Parser NumExp
parseProd = do
    v1 <- vectorexp
    reservedOp mathLang ".*"
    v2 <- vectorexp
    return (VecDot v1 v2)

parseNorm :: Parser NumExp
parseNorm = do
    reserved mathLang "norm"
    symbol mathLang "("
    v <- vectorexp
    comma mathLang
    param <- stringLiteral mathLang  -- 'inf', '1', '2', etc.
    symbol mathLang ")"
    case param of
        "inf" -> return (VecNormInf v)
        "1"   -> return (VecNorm1 v)
        "2"   -> return (VecNorm2 v)
        _     -> return (VecNorm2 v)  -- default

parseSeqOperations :: Parser NumExp
parseSeqOperations = 
        try (do reserved mathLang "length"
                s <- parens mathLang seqexp
                return (SeqLength s))
    <|> try parseSeqFind
    <|> try parseSeqAccess

parseSeqFind :: Parser NumExp
parseSeqFind = do
    reserved mathLang "find"
    symbol mathLang "("
    seq <- seqexp
    comma mathLang
    val <- numexp
    symbol mathLang ")"
    return (SeqFind seq val)

parseSeqAccess :: Parser NumExp
parseSeqAccess = do
    reserved mathLang "access"
    symbol mathLang "("
    seq <- seqexp
    comma mathLang
    idx <- numexp
    symbol mathLang ")"
    return (SeqAccess seq idx)

parseNumber :: Parser NumExp
parseNumber = do
    isRangeAfter <- lookAhead ( optionMaybe ( try ( do
        many1 digit
        string ".." )))
    
    case isRangeAfter of 
        Just _ -> do
            n <- natural mathLang
            return (Const (I (fromInteger n)))
        Nothing -> do
            n <- naturalOrFloat mathLang
            case n of
                Left i  -> return (Const (I (fromInteger i)))
                Right f -> return (Const (F (realToFrac f)))

parseFunCallNum :: Parser NumExp
parseFunCallNum = do
    name <- identifier mathLang
    args <- parens mathLang (commaSep mathLang expr)
    return (FunCallNum name args)


parseVarOrAccess :: Parser NumExp
parseVarOrAccess = try ( do 
        var <- identifier mathLang
        parseAccessOrVar var)

parseAccessOrVar :: String -> Parser NumExp
parseAccessOrVar var = do
    maybeOpenBracket <- optionMaybe (symbol mathLang "[")
    case maybeOpenBracket of
        Nothing -> 
            return (VarNum var)
        
        Just _ -> do
            (try (parseMatAccessRest var) 
             <|> try (parseVecAccessRest var)
             <|> fail "Slice detectado") 

parseMatAccessRest var = do
    i <- numexp
    comma mathLang
    j <- numexp
    symbol mathLang "]"
    return (MatAccess (VarMat var) i j)

parseVecAccessRest var = do
    i <- numexp
    symbol mathLang "]"
    return (VecAccess (VarVec var) i)

-- Parser de expresiones booleanas

boolexp :: Parser BoolExp
boolexp = chainl1 boolexp2 oropp

boolexp2 = chainl1 boolexp3 andopp

boolexp3 = try (parens mathLang boolexp)
       <|> try (do reservedOp mathLang "!"
                   b <- boolexp3
                   return (Not b))
       <|> try parseBoolConstants
       <|> try parseComparison
       <|> try parseSetMember
       <|> try parseSeqContains
       <|> try parseQuantifiers
       <|> try parseFunCallBool
       <|> try parseVarBool

oropp = do try (reservedOp mathLang "||")
           return Or

andopp = do try (reservedOp mathLang "&&")
            return And

parseBoolConstants :: Parser BoolExp
parseBoolConstants = 
        (do reserved mathLang "true"
            return BTrue)
    <|> (do reserved mathLang "false"
            return BFalse)

parseComparison :: Parser BoolExp
parseComparison = do
    e1 <- numexp
    op <- parseCompOp
    e2 <- numexp
    return (op e1 e2)

parseCompOp :: Parser (NumExp -> NumExp -> BoolExp)
parseCompOp = 
        try (do reservedOp mathLang "="
                return Eq)
    <|> try (do reservedOp mathLang "!="
                return Neq)
    <|> try (do reservedOp mathLang "<="
                return Lte)
    <|> try (do reservedOp mathLang ">="
                return Gte)
    <|> try (do reservedOp mathLang "<"
                return Lt)
    <|> try (do reservedOp mathLang ">"
                return Gt)

parseSetMember :: Parser BoolExp
parseSetMember = do
    e <- numexp
    reserved mathLang "in"
    s <- setexp
    return (SetMember s e)

parseSeqContains :: Parser BoolExp
parseSeqContains = do
    s <- seqexp
    reserved mathLang "contains"
    e <- numexp
    return (SeqContains s e)

parseQuantifiers :: Parser BoolExp
parseQuantifiers = 
        try parseForall
    <|> try parseExists

parseForall :: Parser BoolExp
parseForall = do
    reserved mathLang "forall"
    var <- identifier mathLang
    reserved mathLang "in"
    set <- setexp
    comma mathLang
    cond <- boolexp
    return (Forall var set cond)

parseExists :: Parser BoolExp
parseExists = do
    reserved mathLang "exists"
    var <- identifier mathLang
    reserved mathLang "in"
    set <- setexp
    comma mathLang
    cond <- boolexp
    return (Exists var set cond)

parseFunCallBool :: Parser BoolExp
parseFunCallBool = do
    name <- identifier mathLang
    args <- parens mathLang (commaSep mathLang expr)
    return (FunCallBool name args)

parseVarBool :: Parser BoolExp
parseVarBool = do
    var <- identifier mathLang
    notFollowedBy (symbol mathLang "("
               <|> symbol mathLang "["
               <|> symbol mathLang "<") 
    return (VarBool var)

-- Parser de expresiones de conjuntos

setexp :: Parser SetExp
setexp = try parseSetCompr 
     <|> try parseRange 
     <|> try parseSetLit
     <|> try parseSetFunctions
     <|> try parseFunCallSet
     <|> try parseVarSet
     <|> parens mathLang setexp

parseSetCompr :: Parser SetExp 
parseSetCompr = do 
  symbol mathLang "{"
  var <- identifier mathLang
  symbol mathLang "|"
  gen <- parseGenerator 
  conds <- many (do 
              comma mathLang 
              boolexp)
  symbol mathLang "}" 
  return (SetCompr var gen conds)

parseGenerator :: Parser Generator
parseGenerator = do
    var <- identifier mathLang
    reservedOp mathLang "<-"
    set <- setexp
    return (Gen var set )

parseRange :: Parser SetExp
parseRange = braces mathLang $ do
    start <- numexp
    step <- optionMaybe (do comma mathLang
                            numexp)
    symbol mathLang ".."
    end <- numexp
    return (Range start end step)

parseSetLit :: Parser SetExp
parseSetLit = do
    elems <- braces mathLang (commaSep mathLang numexp)
    return (SetLit elems)

parseSetFunctions :: Parser SetExp
parseSetFunctions = 
        try parseUnion
    <|> try parseIntersection
    <|> try parseDifference
    <|> try parseSymmetricDiff
    <|> try parseInsert
    <|> try parseRemove

parseUnion :: Parser SetExp
parseUnion = do
    reserved mathLang "union"
    symbol mathLang "("
    s1 <- setexp
    comma mathLang
    s2 <- setexp
    symbol mathLang ")"
    return (Union s1 s2)

parseIntersection :: Parser SetExp
parseIntersection = do
    reserved mathLang "intersection"
    symbol mathLang "("
    s1 <- setexp
    comma mathLang
    s2 <- setexp
    symbol mathLang ")"
    return (Inter s1 s2)

parseDifference :: Parser SetExp
parseDifference = do
    reserved mathLang "difference"
    symbol mathLang "("
    s1 <- setexp
    comma mathLang
    s2 <- setexp
    symbol mathLang ")"
    return (Diff s1 s2)

parseSymmetricDiff :: Parser SetExp
parseSymmetricDiff = do
    reserved mathLang "symmetricdiff"
    symbol mathLang "("
    s1 <- setexp
    comma mathLang
    s2 <- setexp
    symbol mathLang ")"
    return (SymDiff s1 s2)

parseInsert :: Parser SetExp
parseInsert = do
    reserved mathLang "insert"
    symbol mathLang "("
    var <- identifier mathLang
    comma mathLang
    elem <- numexp
    symbol mathLang ")"
    return (Insert var elem)

parseRemove :: Parser SetExp
parseRemove = do
    reserved mathLang "remove"
    symbol mathLang "("
    var <- identifier mathLang
    comma mathLang
    elem <- numexp
    symbol mathLang ")"
    return (Remove var elem)


parseFunCallSet :: Parser SetExp
parseFunCallSet = do
    name <- identifier mathLang
    args <- parens mathLang (commaSep mathLang expr)
    return (FunCallSet name args)

parseVarSet :: Parser SetExp
parseVarSet = do
    var <- identifier mathLang
    notFollowedBy (symbol mathLang "("
               <|> symbol mathLang "["
               <|> symbol mathLang "<")
    return (VarSet var)

-- Parser de expresiones de matrices

matrixexp :: Parser MatrixExp
matrixexp = chainl1 matterm addmatopp

matterm = chainl1 matfactor mulmatopp

matfactor = try parseMatLit
        <|> try parseMatZeros
        <|> try parseMatOnes
        <|> try parseMatEye
        <|> try parseMatSlice
        <|> try parseMatTrans
        <|> try parseMatDiag
        <|> try parseMatTriU
        <|> try parseMatTriL
        <|> try parseMatSwapRows
        <|> try parseMatSwapCols
        <|> try parseFunCallMat
        <|> try parseVarMat
        <|> parens mathLang matrixexp

mulmatopp = do try (reservedOp mathLang "#*")
               return MatMul

addmatopp = do try (reservedOp mathLang "#+")
               return MatAdd
            <|> do try (reservedOp mathLang "#-")
                   return MatSub

parseMatLit :: Parser MatrixExp
parseMatLit = do
    symbol mathLang "["
    rows <- semiSep1 mathLang (commaSep1 mathLang numexp)
    symbol mathLang "]"
    return (MatLit rows)

parseMatZeros :: Parser MatrixExp
parseMatZeros = do
    reserved mathLang "zeros"
    symbol mathLang "("
    m <- numexp
    comma mathLang
    n <- numexp
    symbol mathLang ")"
    return (MatZeros m n)

parseMatOnes :: Parser MatrixExp
parseMatOnes = do
    reserved mathLang "ones"
    symbol mathLang "("
    m <- numexp
    comma mathLang
    n <- numexp
    symbol mathLang ")"
    return (MatOnes m n)

parseMatEye :: Parser MatrixExp
parseMatEye = do
    reserved mathLang "eye"
    symbol mathLang "("
    n <- numexp
    symbol mathLang ")"
    return (MatEye n)

parseMatSlice :: Parser MatrixExp
parseMatSlice = do
    var <- identifier mathLang  -- Solo variables (POR ALGUNA RAZÓN?)
    symbol mathLang "["
    i1 <- numexp
    reservedOp mathLang ":"
    i2 <- numexp
    comma mathLang
    j1 <- numexp
    reservedOp mathLang ":"
    j2 <- numexp
    symbol mathLang "]"
    return (MatSlice (VarMat var) i1 i2 j1 j2)

parseMatTrans :: Parser MatrixExp
parseMatTrans = do
    reserved mathLang "trans"
    m <- parens mathLang matrixexp
    return (MatTrans m)

parseMatDiag :: Parser MatrixExp
parseMatDiag = do
    reserved mathLang "diag"
    m <- parens mathLang matrixexp
    return (MatDiag m)

parseMatTriU :: Parser MatrixExp
parseMatTriU = do
    reserved mathLang "triu"
    m <- parens mathLang matrixexp
    return (MatTriU m)

parseMatTriL :: Parser MatrixExp
parseMatTriL = do
    reserved mathLang "tril"
    m <- parens mathLang matrixexp
    return (MatTriL m)

parseMatSwapRows :: Parser MatrixExp
parseMatSwapRows = do
    reserved mathLang "swaprows"
    symbol mathLang "("
    mat <- matrixexp
    comma mathLang
    i <- numexp
    comma mathLang
    j <- numexp
    symbol mathLang ")"
    return (MatSwapRows mat i j)

parseMatSwapCols :: Parser MatrixExp
parseMatSwapCols = do
    reserved mathLang "swapcols"
    symbol mathLang "("
    mat <- matrixexp
    comma mathLang
    i <- numexp
    comma mathLang
    j <- numexp
    symbol mathLang ")"
    return (MatSwapCols mat i j)


parseFunCallMat :: Parser MatrixExp
parseFunCallMat = do
    name <- identifier mathLang
    args <- parens mathLang (commaSep mathLang expr)
    return (FunCallMat name args)

parseVarMat :: Parser MatrixExp
parseVarMat = do
    var <- identifier mathLang
    notFollowedBy (symbol mathLang "("
               <|> symbol mathLang "["
               <|> symbol mathLang "<")
    return (VarMat var)

-- Parser de expresiones de vectores

vectorexp :: Parser VectorExp
vectorexp = chainl1 vecterm addvecop

vecterm :: Parser VectorExp
vecterm = do
    base <- vecfactor
    maybeMat <- optionMaybe (try (do
        reservedOp mathLang ".#*"
        mat <- matfactor
        return mat))
    case maybeMat of
        Nothing -> return base
        Just mat -> return (VecMatMul base mat)

addvecop = do try (reservedOp mathLang ".+")
              return VecAdd
           <|> do try (reservedOp mathLang ".-")
                  return VecSub

vecfactor = try parseVecLit
        <|> try parseVecZeros
        <|> try parseScal
        <|> try parseVecSlice
        <|> try parseMatRow
        <|> try parseMatCol
        <|> try parseMatVecMul
        <|> try parseFunCallVec
        <|> try parseVarVec
        <|> parens mathLang vectorexp

parseVecLit :: Parser VectorExp
parseVecLit = try( do
    symbol mathLang "("
    first <- numexp
    comma mathLang
    rest <- option [] (commaSep1 mathLang numexp)
    symbol mathLang ")"
    return (VecLit (first:rest)))

parseVecZeros :: Parser VectorExp
parseVecZeros = do
    reserved mathLang "zeros"
    symbol mathLang "("
    n <- numexp
    symbol mathLang ")"
    return (VecZeros n)

parseScal :: Parser VectorExp
parseScal = do
    reserved mathLang "scal"
    symbol mathLang "("
    scalar <- numexp
    comma mathLang
    vec <- vectorexp
    symbol mathLang ")"
    return (VecScale scalar vec)

parseVecSlice :: Parser VectorExp
parseVecSlice = do
    var <- identifier mathLang  -- Solo variables
    symbol mathLang "["
    i1 <- numexp
    reservedOp mathLang ":"
    i2 <- numexp
    symbol mathLang "]"
    return (VecSlice (VarVec var) i1 i2)


parseMatRow :: Parser VectorExp
parseMatRow = do
    var <- identifier mathLang  -- Solo variables
    symbol mathLang "["
    i <- numexp
    comma mathLang
    reservedOp mathLang ":"
    symbol mathLang "]"
    return (MatRow (VarMat var) i)

parseMatCol :: Parser VectorExp
parseMatCol = do
    var <- identifier mathLang  -- Solo variables
    symbol mathLang "["
    reservedOp mathLang ":"
    comma mathLang
    j <- numexp
    symbol mathLang "]"
    return (MatCol (VarMat var) j)

parseMatVecMul :: Parser VectorExp
parseMatVecMul = try (do
    mat <- matfactor
    reservedOp mathLang "#.*"
    vec <- vecfactor
    return (MatVecMul mat vec))

parseFunCallVec :: Parser VectorExp
parseFunCallVec = do
    name <- identifier mathLang
    args <- parens mathLang (commaSep mathLang expr)
    return (FunCallVec name args)

parseVarVec :: Parser VectorExp
parseVarVec = try (do
    var <- identifier mathLang
    notFollowedBy (symbol mathLang "("
               <|> symbol mathLang "["
               <|> symbol mathLang "<")
    return (VarVec var))

-- Parser de expresiones de strings

strexp :: Parser StrExp
strexp = try parseStrLit
     <|> try parseFunCallStr
     <|> try parseVarStr
     <|> parens mathLang strexp

parseStrLit :: Parser StrExp
parseStrLit = do
    s <- stringLiteral mathLang
    return (StrLit s)

parseFunCallStr :: Parser StrExp
parseFunCallStr = do
    name <- identifier mathLang
    args <- parens mathLang (commaSep mathLang expr)
    return (FunCallStr name args)

parseVarStr :: Parser StrExp
parseVarStr = try (do
    var <- identifier mathLang
    notFollowedBy (symbol mathLang "("
               <|> symbol mathLang "["
               <|> symbol mathLang "<")
    return (VarStr var))

-- Parser de expresiones de secuencias

seqexp :: Parser SeqExp
seqexp = try parseSeqLit
     <|> try parseSeqSlice
     <|> try parseSeqConcat
     <|> try parseSeqAppend
     <|> try parseSeqPrepend
     <|> try parseSeqFilter
     <|> try parseFunCallSeq
     <|> try parseVarSeq
     <|> parens mathLang seqexp

parseSeqLit :: Parser SeqExp
parseSeqLit = do
    symbol mathLang "<"
    elems <- commaSep mathLang numexp
    symbol mathLang ">"
    return (SeqLit elems)

parseSeqSlice :: Parser SeqExp
parseSeqSlice = do
    reserved mathLang "slice"
    symbol mathLang "("
    seq <- seqexp
    comma mathLang
    i1 <- numexp
    comma mathLang
    i2 <- numexp
    symbol mathLang ")"
    return (SeqSlice seq i1 i2)

parseSeqConcat :: Parser SeqExp
parseSeqConcat = do
    reserved mathLang "concat"
    symbol mathLang "("
    s1 <- seqexp
    comma mathLang
    s2 <- seqexp
    symbol mathLang ")"
    return (SeqConcat s1 s2)

parseSeqAppend :: Parser SeqExp
parseSeqAppend = do
    reserved mathLang "append"
    symbol mathLang "("
    seq <- seqexp
    comma mathLang
    elem <- numexp
    symbol mathLang ")"
    return (SeqAppend seq elem)

parseSeqPrepend :: Parser SeqExp
parseSeqPrepend = do
    reserved mathLang "prepend"
    symbol mathLang "("
    elem <- numexp
    comma mathLang
    seq <- seqexp
    symbol mathLang ")"
    return (SeqPrepend seq elem)

parseSeqFilter :: Parser SeqExp
parseSeqFilter = do
    reserved mathLang "filter"
    symbol mathLang "("
    var <- identifier mathLang
    comma mathLang
    seq <- seqexp
    comma mathLang
    cond <- boolexp
    symbol mathLang ")"
    return (SeqFilter var seq cond)

parseFunCallSeq :: Parser SeqExp
parseFunCallSeq = do
    name <- identifier mathLang
    args <- parens mathLang (commaSep mathLang expr)
    return (FunCallSeq name args)

parseVarSeq :: Parser SeqExp
parseVarSeq = try (do
    var <- identifier mathLang
    notFollowedBy (symbol mathLang "("
               <|> symbol mathLang "["
               <|> symbol mathLang "<")
    return (VarSeq var))

-- Parser de expresiones generales

expr :: Parser Exp
expr = try parseVar
   <|> try parseFunCall
   <|> try (do e <- boolexp
               return (EBool e))
   <|> try (do e <- numexp
               return (ENum e))
   <|> try (do e <- vectorexp
               return (EVector e))
   <|> try (do e <- matrixexp
               return (EMatrix e))
   <|> try (do e <- setexp
               return (ESet e))
   <|> try (do e <- seqexp
               return (ESeq e))
   <|> (do e <- strexp
           return (EStr e))

data OperatorType = MatrixOp | VectorOp | ScalarOp | BoolOp

detectOperatorType :: Parser OperatorType
detectOperatorType = choice [
    try (reservedOp mathLang "#+" >> return MatrixOp),
    try (reservedOp mathLang "#-" >> return MatrixOp),
    try (reservedOp mathLang "#*" >> return MatrixOp),
    try (reservedOp mathLang "#.*" >> return VectorOp),
    try (reservedOp mathLang ".+" >> return VectorOp),
    try (reservedOp mathLang ".-" >> return VectorOp),
    try (reservedOp mathLang ".#*" >> return VectorOp),
    try (reservedOp mathLang ".*" >> return VectorOp),
    try (reservedOp mathLang "+" >> return ScalarOp),
    try (reservedOp mathLang "-" >> return ScalarOp),
    try (reservedOp mathLang "*" >> return ScalarOp),
    try (reservedOp mathLang "/" >> return ScalarOp),
    try (reservedOp mathLang "^" >> return ScalarOp),
    try (reserved mathLang "mod" >> return ScalarOp),
    try (reservedOp mathLang "&&" >> return BoolOp),
    try (reservedOp mathLang "||" >> return BoolOp),
    try (reservedOp mathLang "=" >> return BoolOp),
    try (reservedOp mathLang "!=" >> return BoolOp),
    try (reservedOp mathLang "<" >> return BoolOp),
    try (reservedOp mathLang "<=" >> return BoolOp),
    try (reservedOp mathLang ">" >> return BoolOp),
    try (reservedOp mathLang ">=" >> return BoolOp),
    try (reserved mathLang "in" >> return BoolOp),
    try (reserved mathLang "contains" >> return BoolOp)
    ]

parseFunCall :: Parser Exp
parseFunCall = do
    _ <- lookAhead (do
        identifier mathLang
        symbol mathLang "(")
    
    opType <- lookAhead $ optionMaybe (try (do
        identifier mathLang
        parens mathLang (commaSep mathLang expr)
        detectOperatorType))
    
    case opType of
        Just MatrixOp -> do
            e <- matrixexp
            return (EMatrix e)
        Just VectorOp -> do
            e <- vectorexp
            return (EVector e)
        Just ScalarOp -> do
            e <- numexp
            return (ENum e)
        Just BoolOp -> do
            e <- boolexp
            return (EBool e)
        _ -> do
            -- No hay operador, parsear función genérica
            name <- identifier mathLang
            args <- parens mathLang (commaSep mathLang expr)
            return (FunCall name args)

parseVar :: Parser Exp
parseVar = do
    var <- lookAhead (identifier mathLang)
    notFollowedByAccess <- lookAhead $ optionMaybe (try (do
        identifier mathLang
        choice [symbol mathLang "(", symbol mathLang "["]))
    case notFollowedByAccess of
        Just _ -> fail "identifier followed by ( or ["
        Nothing -> do
            opType <- lookAhead $ optionMaybe (try (do
                identifier mathLang
                detectOperatorType))
            case opType of
                Just MatrixOp -> do
                    e <- matrixexp
                    return (EMatrix e)
                Just VectorOp -> do
                    e <- vectorexp
                    return (EVector e)
                Just ScalarOp -> do
                    e <- numexp
                    return (ENum e)
                Just BoolOp -> do
                    e <- boolexp
                    return (EBool e)
                _ -> do
                    v <- identifier mathLang
                    return (EVar v)

-- Parser de comandos

comm :: Parser Comm
comm = do
    cmds <- sepEndBy1 comm2 (reservedOp mathLang ";")
    return $ foldr1 SeqComm cmds

comm2 :: Parser Comm
comm2 = try parseSkip
    <|> try parseIf
    <|> try parseWhile
    <|> try parseFor
    <|> try parseForEach
    <|> try parseFuncDef
    <|> try parsePrint
    <|> try parseError
    <|> try parseAssign

parseSkip :: Parser Comm
parseSkip = do
    reserved mathLang "skip"
    return Skip

parseAssign :: Parser Comm
parseAssign = do
    var <- identifier mathLang
    reservedOp mathLang ":="
    e <- expr
    return (Assign var e)

parseIf :: Parser Comm
parseIf = do
    reserved mathLang "if"
    cond <- boolexp
    reserved mathLang "then"
    c1 <- comm
    optional (reservedOp mathLang ";")
    c2 <- optionMaybe (do
        reserved mathLang "else"
        c <- comm
        optional (reservedOp mathLang ";")
        return c)
    reserved mathLang "end"
    return (If cond c1 c2)

parseWhile :: Parser Comm
parseWhile = do
    reserved mathLang "while"
    cond <- boolexp
    reserved mathLang "do"
    body <- comm
    optional (reservedOp mathLang ";")
    reserved mathLang "end"
    return (While cond body)

parseFor :: Parser Comm
parseFor = do
    reserved mathLang "for"
    start <- parseAssign
    reservedOp mathLang ":"
    cond <- boolexp
    reservedOp mathLang ":"
    step <- parseAssign
    reserved mathLang "do"
    body <- comm
    optional (reservedOp mathLang ";")
    reserved mathLang "end"
    return (For start cond step body)

parseForEach :: Parser Comm
parseForEach = do
    reserved mathLang "foreach"
    var <- identifier mathLang
    reserved mathLang "in"
    seq <- seqexp
    reserved mathLang "do"
    body <- comm
    optional (reservedOp mathLang ";")
    reserved mathLang "end"
    return (ForEach var seq body)

parseFuncDef :: Parser Comm
parseFuncDef = do
    reserved mathLang "def"
    name <- identifier mathLang
    params <- parens mathLang (commaSep mathLang (identifier mathLang))
    reserved mathLang "do"
    body <- try (do
        reserved mathLang "return"
        retExpr <- expr
        optional (reservedOp mathLang ";")
        reserved mathLang "end"
        return (name, params, Skip, retExpr))
        <|> (do
        c <- comm
        optional (reservedOp mathLang ";")
        reserved mathLang "return"
        retExpr <- expr
        optional (reservedOp mathLang ";")
        reserved mathLang "end"
        return (name, params, c, retExpr))
    let (n, p, b, r) = body
    return (FuncDef n p b r)

parsePrint :: Parser Comm
parsePrint = do
    reserved mathLang "print"
    es <- parens mathLang (commaSep1 mathLang expr)
    return (Print es)

parseError :: Parser Comm
parseError = do
    reserved mathLang "error"
    msg <- parens mathLang expr
    return (Error msg)

parseComm :: SourceName -> String -> Either ParseError Comm
parseComm = parse (totParser comm)

parseExpr :: SourceName -> String -> Either ParseError Exp
parseExpr = parse (totParser expr)
