{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoFieldSelectors #-}

module Main where

import qualified Check
import Data.String (fromString)
import qualified Data.Text as Text
import qualified Data.Text.IO as Text.IO
import qualified Data.Text.Lazy.IO as Text.Lazy.IO
import qualified Options.Applicative as Options
import qualified Parse
import qualified Syntax
import System.Exit (exitFailure)
import System.IO (hPrint, stderr)
import qualified Text.Diagnostic as Diagnostic
import qualified Type
import Prelude hiding (span)

data Cli = Cli {file :: FilePath}

cliParser :: Options.Parser Cli
cliParser =
  Cli <$> Options.strArgument (Options.metavar "FILE")

reportCheckError :: FilePath -> Check.Error -> IO a
reportCheckError fileName err = do
  let report =
        case err of
          Check.NotInScope span ->
            Diagnostic.emit (Diagnostic.Offset span.pos) (Diagnostic.Span span.len) "variable not in scope"
          Check.TypeMismatch{Check.span, Check.expected, Check.actual} ->
            Diagnostic.emit (Diagnostic.Offset span.pos) (Diagnostic.Span span.len) . Diagnostic.Message $
              "expected type " <> fromString (Type.display expected) <> ", got type "
                <> fromString (Type.display actual)
          Check.Occurs{Check.span, Check.meta, Check.ty} ->
            Diagnostic.emit (Diagnostic.Offset span.pos) (Diagnostic.Span span.len) . Diagnostic.Message $
              "infinite type from equating " <> fromString ("?" <> show meta) <> " with "
                <> fromString (Type.display ty)
  fileContents <- Text.IO.readFile fileName
  Text.Lazy.IO.hPutStrLn stderr $
    Diagnostic.render
      Diagnostic.defaultConfig
      (Text.pack fileName)
      fileContents
      report
  exitFailure

main :: IO ()
main = do
  cli <- Options.execParser $ Options.info cliParser Options.fullDesc
  expr <- Parse.fromFile cli.file Parse.expr
  (core, ty) <- either (reportCheckError cli.file) pure =<< Check.runCheckT mempty (Check.infer expr)
  hPrint stderr core
  hPrint stderr ty