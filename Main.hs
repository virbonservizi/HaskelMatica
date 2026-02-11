module Main where

import System.Environment (getArgs, getProgName)
import Parser (parseComm, parseExpr)
import Eval (eval)
import AST (Comm(..))

main :: IO ()
main = do 
    args <- getArgs
    case args of
        [] -> print("Uso: run programa <archivo> [arg1 arg2 ...]")
        (file:literalArgs) -> run file literalArgs

run :: FilePath -> [String] -> IO ()
run file literalArgs = do
    s <- readFile file
    
    case parseComm file s of
        Left parseError -> error ("Error de parsing: " ++ show parseError)
        Right ast -> do
            finalAst <- if null literalArgs
                then return ast
                else do
                    assignments <- createArgAssignments literalArgs
                    return $ SeqComm assignments ast
            print(finalAst)
            result <- eval finalAst
            case result of
                Left evalError -> do
                    putStrLn("Ejecución fallida.")
                    putStrLn(evalError)
                Right _ -> putStrLn "Ejecución completada."

-- Crea asignaciones arg1 := <valor>, arg2 := <valor>, etc.
createArgAssignments :: [String] -> IO Comm
createArgAssignments args = do
    case mapM parseArg (zip [1..] args) of
        Left err -> error err
        Right assigns -> return $ foldr1 SeqComm assigns

-- Parsea un argumento y crea una asignación argN := <expresión>
parseArg :: (Int, String) -> Either String Comm
parseArg (n, arg) = 
    case parseExpr "<argumento>" arg of
        Left parseError -> Left $ "Error al parsear argumento " ++ show n ++ 
                                   " (" ++ arg ++ "): " ++ show parseError
        Right expr -> Right $ Assign ("arg" ++ show n) expr
