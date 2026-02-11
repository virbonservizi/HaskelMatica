module AST where

type Variable = String

data Number = I Int | F Float
              deriving (Show, Eq)

instance Ord Number where
    compare (I a) (I b) = compare a b
    compare (F a) (F b) = compare a b
    compare (I a) (F b) = compare (fromIntegral a :: Float) b
    compare (F a) (I b) = compare a (fromIntegral b :: Float)

data NumExp  = Const Number
             | VarNum Variable 
             | Pi
             | E
             | Neg NumExp
             | Add NumExp NumExp
             | Sub NumExp NumExp
             | Mul NumExp NumExp
             | Div NumExp NumExp
             | Mod NumExp NumExp
             | Pow NumExp NumExp
             | Abs NumExp
             | Sqrt NumExp
             | Sin NumExp
             | Cos NumExp
             | Exp NumExp
             | Log NumExp NumExp
             | Cardinal SetExp
             | SetMin SetExp
             | SetMax SetExp
             | SetSum SetExp
             | MatAccess MatrixExp NumExp NumExp
             | MatRows MatrixExp
             | MatCols MatrixExp
             | VecDot VectorExp VectorExp
             | VecMax VectorExp
             | VecMin VectorExp
             | VecAccess VectorExp NumExp
             | VecNorm1 VectorExp
             | VecNorm2 VectorExp
             | VecNormInf VectorExp
             | SeqAccess SeqExp NumExp
             | SeqLength SeqExp
             | SeqFind SeqExp NumExp
             | FunCallNum Variable [Exp]
             deriving (Show, Eq)

data BoolExp = BTrue
             | BFalse
             | VarBool Variable
             | Eq NumExp NumExp
             | Neq NumExp NumExp
             | Lt NumExp NumExp
             | Lte NumExp NumExp
             | Gt NumExp NumExp
             | Gte NumExp NumExp
             | And BoolExp BoolExp
             | Or BoolExp BoolExp
             | Not BoolExp
             | SetMember SetExp NumExp
             | SeqContains SeqExp NumExp
             | Forall Variable SetExp BoolExp
             | Exists Variable SetExp BoolExp
             | FunCallBool Variable [Exp]
             deriving (Show, Eq)

data SetExp = SetLit [NumExp]
            | VarSet Variable
            | Range NumExp NumExp (Maybe NumExp)
            | Union SetExp SetExp
            | Inter SetExp SetExp
            | Diff SetExp SetExp
            | SymDiff SetExp SetExp
            | SetCompr Variable Generator [BoolExp]
            | Insert Variable NumExp
            | Remove Variable NumExp
            | FunCallSet Variable [Exp]
            deriving (Show, Eq)

data Generator = Gen Variable SetExp
               deriving (Show, Eq)

data MatrixExp = MatLit [[NumExp]]
               | VarMat Variable
               | MatZeros NumExp NumExp
               | MatOnes NumExp NumExp
               | MatEye NumExp
               | MatSlice MatrixExp NumExp NumExp NumExp NumExp
               | MatAdd MatrixExp MatrixExp
               | MatSub MatrixExp MatrixExp
               | MatMul MatrixExp MatrixExp
               | MatTrans MatrixExp
               | MatDiag MatrixExp
               | MatTriU MatrixExp
               | MatTriL MatrixExp
               | MatSwapRows MatrixExp NumExp NumExp
               | MatSwapCols MatrixExp NumExp NumExp
               | FunCallMat Variable [Exp] 
               deriving (Show, Eq)

data VectorExp = VecLit [NumExp]
               | VarVec Variable
               | VecZeros NumExp
               | VecSlice VectorExp NumExp NumExp
               | VecAdd VectorExp VectorExp        
               | VecSub VectorExp VectorExp
               | VecScale NumExp VectorExp
               | MatRow MatrixExp NumExp                   
               | MatCol MatrixExp NumExp
               | MatVecMul MatrixExp VectorExp
               | VecMatMul VectorExp MatrixExp
               | FunCallVec Variable [Exp]
               deriving (Show, Eq)

data SeqExp = SeqLit [NumExp]
            | VarSeq Variable
            | SeqSlice SeqExp NumExp NumExp
            | SeqConcat SeqExp SeqExp
            | SeqAppend SeqExp NumExp
            | SeqPrepend SeqExp NumExp
            | SeqFilter Variable SeqExp BoolExp
            | FunCallSeq Variable [Exp]
            deriving (Show, Eq)

data StrExp = StrLit String
            | VarStr Variable
            | FunCallStr Variable [Exp]
            deriving (Show, Eq)

data Comm = Skip
          | Assign Variable Exp
          | SeqComm Comm Comm
          | If BoolExp Comm (Maybe Comm)
          | While BoolExp Comm
          | For Comm BoolExp Comm Comm
          | ForEach Variable SeqExp Comm
          | FuncDef Variable [Variable] Comm Exp 
          | Print [Exp]
          | Error Exp
          deriving (Show, Eq)

data Exp = ENum NumExp
         | EBool BoolExp
         | ESet SetExp
         | EMatrix MatrixExp
         | EVector VectorExp
         | ESeq SeqExp
         | EStr StrExp
         | EVar Variable
         | FunCall Variable [Exp]
          deriving (Show, Eq)