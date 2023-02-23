{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}

module Main (main) where

import Options.Applicative (execParser,Parser,header,progDesc,fullDesc,helper)
import Options.Applicative (info,metavar,argument)
import Options.Applicative (many,(<**>))

import Control.Monad (forM_)
import Data.List.Snoc (Tsil, pattern Snoc, pattern Nil)
import Data.Text (Text)
import Text.Newline (linesUnix)

import qualified Data.List.Snoc as Snoc
import qualified Data.Text as T
import qualified Data.Text.IO as T
import qualified Options.Applicative as Opts

------------ Main ------------

main :: IO ()
main = do
  opts <- getOpts
  case opts.files of
    [] -> mainStdinout opts
    fs -> forM_ fs $ mainFilepath opts
  where
  mainStdinout _ = do
    inp <- T.getContents
    let !outp = autotab inp
    T.putStr outp
  mainFilepath _ filepath = do
    inp <- T.readFile filepath
    let !outp = autotab inp
    T.writeFile filepath outp

------------ Autotab ------------

-- TODO right now, this recognizes only Unix line endings and ensures there's a trailing newline
autotab :: Text -> Text
autotab = goSearch . linesUnix
  where
  goSearch :: [Text] -> Text
  goSearch [] = ""
  goSearch (l:ls)
    | hasColumns l = goAlign Snoc.nil (l:ls)
    | otherwise = l <> "\n" <> goSearch ls
  goAlign :: Tsil (Tsil Text) -> [Text] -> Text
  goAlign acc [] = createTable acc
  goAlign acc (l:ls)
    | hasColumns l = goAlign (acc `Snoc` splitColumns l) ls
    | otherwise = createTable acc <> goSearch (l:ls)

hasColumns :: Text -> Bool
hasColumns str =  " \\ " `T.isInfixOf`  str
               || " \\"  `T.isSuffixOf` str

splitColumns :: Text -> Tsil Text
splitColumns = go Nil
  where
  go acc str = case split str of
    Nothing -> acc `Snoc` str
    Just (pre, post) -> go (acc `Snoc` pre) post
  -- if there is a ` \\ `, return the parts before and after the backslash
  split :: Text -> Maybe (Text, Text)
  split str
    -- NOTE: skip backslash at line-start
    | Just ('\\', rest) <- T.uncons str
    , Just (' ', _) <- T.uncons rest = do
      (pre, post) <- split rest
      pure (T.cons '\\' pre, post)
    -- NOTE: we know that `pre` will not be empty
    | otherwise = do
      let (pre, atpost) = T.break (== '\\') str
      ('\\', post) <- T.uncons atpost
      -- verify surrounding whitespace
      (_, ' ') <- T.unsnoc pre
      () <- case T.uncons post of
        Nothing -> pure ()
        Just (' ', _) -> pure ()
        _ -> Nothing
      -- ok
      pure (T.dropWhileEnd (==' ') pre, post)

createTable :: Elbat Text -> Text
createTable Nil = ""
createTable allLines@(ls `Snoc` l) = createTable ls <> padCols l
  where
  padCols :: Tsil Text -> Text
  padCols Nil = "\n"
  padCols (cols `Snoc` col) = T.concat
    [ T.concat (zipWith padCol colSizes (Snoc.toList cols))
    , col <> "\n"
    ]
  padCol :: Int -> Text -> Text
  padCol n str = str <> T.replicate (n - T.length str) " " <> "\\"
  colSizes :: [Int]
  colSizes = foldr (zipWith max) [0,0..] $
    Snoc.reverseOut (colsToSize <$> allLines)
  colSize :: Text -> Int
  colSize = (+ 1) . T.length
  colsToSize :: Tsil Text -> [Int]
  colsToSize = snocFoldr (\col sizes -> colSize col : sizes) [0,0..]

type Elbat a = Tsil (Tsil a)

------------ Options ------------

data Opts = Opts
  { files :: [FilePath]
  -- , dropWhitespace :: Bool -- TODO do this in a different application
  }
-- TODO -L flag to not normalize line endings
-- TODO -l option to set the type of newline
-- TODO -n flag to normalize trailing newline
-- TODO -w flag to delete trailing whitespace
-- TODO -C flag to disallow aligning trailing backslash?
-- TODO -n,--npad option to set amount of left/right padding generated around a backslash
-- TODO -n -1 "freezes" i.e. remove backslashes

config :: Parser Opts
config = Opts
  <$> many (argument Opts.str
      ( metavar "FILES...") )
  -- <*> flag False True
  --     (  long "delete-trailing-whitespace"
  --     <> short "w"
  --     <> help "deletes whitespace from the end of each line" )

getOpts :: IO Opts
getOpts = execParser $ info
  (config <**> helper)
   ( fullDesc
  <> progDesc "Add spaces to align backslash-delimited tables"
  <> header "autotab - align columns of related source code" )

------------ TODO move to Data.List.Snoc ------------

-- TODO make a Foldable instance for Tsil
-- TODO swap Rlist and Tsil, or even elim RList
-- TODO associate Nil, Snoc with Tsil

snocFoldr :: (a -> b -> b) -> b -> Tsil a -> b
snocFoldr _ z Nil = z
snocFoldr f z (xs `Snoc` x) = snocFoldr f (f x z) xs
