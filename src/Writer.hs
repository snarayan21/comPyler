{-# LANGUAGE RecordWildCards #-}

module Writer where

import Data.Word (Word8, Word16, Word32, Word64)
import System.IO (Handle, openFile, IOMode(..), hClose)
import qualified Data.ByteString.Lazy as B (ByteString, hPutStr, length)
import Data.ByteString.Lazy.UTF8 as UTF8 (fromString)
import Data.Binary.Put (Put, runPut, putWord8, putWord32le, putWord64le, putDoublele, putLazyByteString)
import Data.Char (ord)
import System.FilePath (dropExtension)

data PyCodeObj =
    Code {
    argcount :: Word32, --number of args
    kwargcount :: Word32,
    numlocals :: Word32, --number of local variables
    stacksize :: Word32, --evaluation stack size
    codeflags :: Word32, --flags for code objects (ex: optimized, newlocals, varargs, varkeywords, nested, nofree, etc)
    code :: PyCodeObj, -- opcodes for instructions
    consts :: PyCodeObj, -- constants used
    names :: PyCodeObj, -- list of names used
    varnames :: PyCodeObj, --local variable names
    freevars :: PyCodeObj, --free variable names
    cellvars :: PyCodeObj, --cell variable names
    codefilename :: PyCodeObj, --filename in Unicode
    name :: PyCodeObj, --unicode name
    firstline :: Word32, --first line number from source
    lnotable :: PyCodeObj --string that encodes address to line number mapping
    }
    | String {string :: B.ByteString}
    | Tuple {tup_vals :: [PyCodeObj]}
    | Int {int_val :: Word32}
    | Float {float_val :: Double}
    | Unicode {unicode :: String}
    | TrueVal
    | FalseVal
    | None
    deriving (Eq, Ord, Show)

data PycFile = PycFile {magic :: Word32, mod_time :: Word32, size :: Word32, object :: PyCodeObj} deriving (Show)

writeToPycFile :: PycFile -> FilePath -> IO ()
writeToPycFile pycfileobj srcpath = do
    let pycpath = dropExtension srcpath ++ ".pyc"
    pycfile <- openFile pycpath WriteMode
    let pycbytes = runPut (putPycFile pycfileobj)
    B.hPutStr pycfile pycbytes
    hClose pycfile

putPycFile :: PycFile -> Put
putPycFile pyc = do
    --put the magic, bit field (empty for now), mod time, and size in put buffer first
    putWord32le (magic pyc)
    putWord32le (0 :: Word32)
    putWord32le (mod_time pyc)
    putWord32le (size pyc)
    writeObject (object pyc)

writeObject :: PyCodeObj -> Put
writeObject obj =
    case obj of
        Code {..} -> writeCodeObj obj
        String {..} -> writeStringObj obj
        Tuple {..} -> writeTupleObj obj
        Int {..} -> writeIntObj obj
        Float {..} -> writeFloatObj obj
        Unicode {..} -> writeUnicodeObj obj
        TrueVal -> putWord8 (fromIntegral (ord 'T'))
        FalseVal -> putWord8 (fromIntegral (ord 'F'))
        None -> putWord8 (fromIntegral (ord 'N'))

--Note: mapM_ ignores results. we just want to put objects into binary buffer.
--Note: check all of these types and see if they compile correctly.
--Special attention to CodeObj, Tuple, Float.

writeCodeObj :: PyCodeObj -> Put
writeCodeObj (Code {..}) = do
    --putWord8 (fromIntegral (ord 'c'))
    putWord8 (227 :: Word8)
    putWord32le argcount --may be word32
    putWord32le (0 :: Word32)
    putWord32le (0 :: Word32)
    putWord32le numlocals --may be word32
    --putWord32le kwargcount --> not sure to include this or not?
    putWord32le stacksize
    putWord32le codeflags
    writeObject code
    writeObject consts
    writeObject names
    writeObject varnames
    writeObject freevars
    writeObject cellvars
    writeObject codefilename
    writeObject name
    putWord32le firstline
    writeObject lnotable
writeCodeObj anything = error "writeCodeObj error"

writeStringObj :: PyCodeObj -> Put
writeStringObj (String {..}) = do
    putWord8 (fromIntegral (ord 's'))
    putWord32le (fromIntegral (B.length string))
    putLazyByteString string
writeStringObj anything = error "writeStringObj error"

writeTupleObj :: PyCodeObj -> Put
writeTupleObj (Tuple {..}) = do
    putWord8 (fromIntegral (ord '('))
    putWord32le (fromIntegral (length tup_vals))
    mapM_ writeObject tup_vals
writeTupleObj anything = error "writeTupleObj error"

writeIntObj :: PyCodeObj -> Put
writeIntObj (Int {..}) = do
    putWord8 (fromIntegral (ord 'i'))
    putWord32le int_val
writeIntObj anything = error "writeIntObj error"

writeFloatObj :: PyCodeObj -> Put
writeFloatObj (Float {..}) = do
    putWord8 (fromIntegral (ord 'f'))
    putDoublele float_val
writeFloatObj anything = error "writeFloatObj error"

writeUnicodeObj :: PyCodeObj -> Put
writeUnicodeObj (Unicode {..}) = do
    let unic = UTF8.fromString unicode
    putWord8 (fromIntegral (ord 'u'))
    putWord32le (fromIntegral (B.length unic))
    putLazyByteString unic
writeUnicodeObj anything = error "writeUnicodeObj error"