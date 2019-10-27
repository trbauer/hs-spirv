module TestFiles where

import Language.SPIRV.SPIRV

import Control.Monad
import Data.Int
import Data.List
import Data.Ord
import Debug.Trace
import System.Exit
import System.Directory
import System.IO
import Text.Printf
import qualified Data.ByteString as S

t1 = testTree "tests"
t1v = testTreeV "tests"
t2 = testTree "D:\\wlt-data\\gfx-driver-ci-master-1627"

fatal :: String -> IO a
fatal = die

testTree :: FilePath -> IO ()
testTree = testTreeG False
testTreeV :: FilePath -> IO ()
testTreeV = testTreeG True

testTreeG :: Bool -> FilePath -> IO ()
testTreeG vrb fp_root = traverseTree fp_root testFile
  where testFile spv_fp = do
          putStr (printf "%-64s" spv_fp ++ "   ")
          bs <- S.readFile spv_fp
          case decodeBinary bs of
            Left err -> do
              putStrLn "FAILED\n"
              fatal err
            Right b -> do
              putStrLn "SUCCESS"
              show b`seq` return ()
              when vrb $ do
                print (b{bOps = []})
                mapM_ (\o -> putStrLn $ "  " ++ show o) (bOps b)



traverseTree :: FilePath -> (FilePath -> IO ()) -> IO ()
traverseTree fp_root0 apply = do
  z <- doesFileExist fp_root0
  let fp_root :: FilePath
      fp_root = fp_root0 ++ maybe_slash
        where maybe_slash
                | z || not (null fp_root0) && last fp_root0 `elem` "/\\" = ""
                | otherwise = "/"
  traverseTreeG fp_root apply

traverseTreeG :: FilePath -> (FilePath -> IO ()) -> IO ()
traverseTreeG fp_root apply = travPath ""
  where travPath fp_sfx = do
          let fp = mkFullFp fp_sfx
          z <- doesDirectoryExist fp
          let maybe_slash = if null fp_sfx then "" else "/"
          if z then travDir (fp_sfx++maybe_slash)
            else do
              z <- doesFileExist fp
              if z then travFile fp_sfx
                else fatal (fp ++ ": file not found")

        mkFullFp :: FilePath -> FilePath
        mkFullFp fp_sfx = if null fp_sfx then fp_root else (fp_root ++ "/" ++ fp_sfx)

        travDir fp_sfx = do
          let fp = mkFullFp fp_sfx
          es <- sort <$> listDirectory fp
          mapM_ travPath (map (fp_sfx ++) es)

        travFile fp_sfx = do
          let fp = mkFullFp fp_sfx
          let skip = not (".spv"`isSuffixOf`fp)
          unless skip $ apply fp
