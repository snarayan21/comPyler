module Parser where 

import Language.Python.Common.AST as AST (
    ModuleSpan,
    Module (..),
    Statement (..), 
    Expr (..) )
import System.IO (openFile, hClose, hFileSize, hGetContents, IOMode(..))
import Language.Python.Version3.Parser (parseModule)

parseBodyAndValidate :: String -> FilePath -> IO ModuleSpan
parseBodyAndValidate fileContents sourceName =
   case parseModule fileContents sourceName of
      Left e -> error $ "parse error: " ++ show e
      Right (pyModule, _comments) -> return pyModule

parseFile :: FilePath -> IO ModuleSpan
parseFile path = do
    pyfile <- openFile path ReadMode
    --filesize <- hFileSize pyfile
    pycode <- hGetContents pyfile
    parseBodyAndValidate pycode path

getFileSize :: FilePath -> IO Integer
getFileSize path = do
    pyfile <- openFile path ReadMode
    hFileSize pyfile