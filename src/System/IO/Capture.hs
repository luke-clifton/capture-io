{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
module System.IO.Capture (captureOutput, example) where

import Control.DeepSeq
import Control.Exception
import Control.Monad
import Data.List
import Language.Haskell.Meta.Parse
import Language.Haskell.TH.Quote
import Language.Haskell.TH.Syntax
import System.IO
import System.Posix

data IOMismatch = IOMismatch
    { expected :: String
    , recieved :: String
    }
    deriving (Show)

instance Exception IOMismatch

-- | Take an IO action, and return it's stdout and stderr.
captureOutput :: IO a -> IO String
captureOutput a = do
    (r, w) <- createPipe
    pid <- forkProcess $ (do
        closeFd r
        handle (\SomeException{} -> pure ()) (closeFd stdInput)
        dupTo w stdOutput
        dupTo w stdError
        void a
        ) `finally` closeFd w
    
    closeFd w
    res <- fdToHandle r >>= hGetContents
    deepseq res $ getProcessStatus True False pid
    pure res

-- | Similar to @`captureOutput`@, but more similar to how GHCi might
-- display it.
captureWithResult :: Show a => IO a -> IO String
captureWithResult a = captureOutput (a >>= print)

codePrefix :: String
codePrefix = ">> "

blocks :: [String] -> [String]
blocks = map (unlines . map removePrefix) . groupBy (\a b -> ">> " `isPrefixOf` b == ">> " `isPrefixOf` a) . dropWhile (not . (">> " `isPrefixOf`)) . filter (not . ("#line " `isPrefixOf`))

removePrefix :: String -> String
removePrefix s = if ">> " `isPrefixOf` s then "  " ++ drop 3 s else s

grouped :: [String] -> [(String, String)]
grouped [] = []
grouped [a] = [(a, "")]
grouped (a:b:rest) = (a, b) : grouped rest

toExp :: (String, String) -> Q Exp
toExp (code, result) = do
    let Right e = parseExp ("do\n" ++ code)
    [| captureOutput $(pure e) >>= \res -> when (res /= result) (throwIO $ IOMismatch result res) |]

-- | A QuasiQuoter that can be used for writing examples that might look like
-- an interactive GHCi session. The code is executed as individual blocks
-- grouped by a @>> @ prefix. You can bind variables within a block, but all
-- of them go out of scope for the next block. The text directly after a block
-- is the expected result. If it does not match, an IOMismatch error is thrown.
--
-- This is particularly useful for READMEs.
--
-- @@
--  >> print 5
--  5
-- @@
--
-- @@
--  >> Just home <- getEnv "HOME"
--  >> putStrLn $ "Your home is: " ++ home
--  Your home is: /home/lukec
-- @@
example :: QuasiQuoter
example = QuasiQuoter
    { quoteExp = \s -> do
        es <- mapM toExp $ grouped $ blocks (lines s)
        p <- [| pure () |]
        pure $ foldl1 (\a b -> AppE (AppE (VarE (mkName ">>")) a) b) es
    }  

