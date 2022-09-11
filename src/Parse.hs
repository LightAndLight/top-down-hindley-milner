{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Parse where

import Control.Applicative (many, optional, (<|>))
import Control.Monad.IO.Class (MonadIO, liftIO)
import Syntax (Expr (..), Span (..), Spanned (..))
import System.Exit (exitFailure)
import System.IO (hPrint, stderr)
import Text.Parser.Char (CharParsing, alphaNum, lower, upper)
import Text.Parser.Token (IdentifierStyle (..), TokenParsing, Unspaced (..), integer, parens, stringLiteral, symbol, symbolic, whiteSpace)
import qualified Text.Parser.Token
import qualified Text.Parser.Token.Highlight
import Text.Trifecta (DeltaParsing (..), ErrInfo (..), Result (..), position)
import Text.Trifecta.Delta (bytes)
import Text.Trifecta.Parser (Parser, parseFromFileEx)
import Type (Type)
import qualified Type
import Prelude hiding (span)

fromFile :: MonadIO m => FilePath -> Parser a -> m a
fromFile file parser = do
  result <- parseFromFileEx parser file
  case result of
    Failure err ->
      liftIO $ do
        hPrint stderr $ _errDoc err
        exitFailure
    Success a ->
      pure a

idStyle :: CharParsing m => IdentifierStyle m
idStyle =
  IdentifierStyle
    { _styleName = "ident"
    , _styleStart = lower <|> upper
    , _styleLetter = alphaNum
    , _styleReserved = ["with", "in", "forall", "if", "then", "else"]
    , _styleHighlight = Text.Parser.Token.Highlight.Identifier
    , _styleReservedHighlight = Text.Parser.Token.Highlight.ReservedIdentifier
    }

ident :: (Monad m, TokenParsing m) => m String
ident =
  Text.Parser.Token.ident idStyle

reserved :: (Monad m, TokenParsing m) => String -> m ()
reserved =
  Text.Parser.Token.reserve idStyle

spanned :: DeltaParsing m => m a -> m (Spanned a)
spanned m = do
  startDelta <- position
  a <- m
  endDelta <- position
  whiteSpace
  let !start = fromIntegral $ bytes startDelta
  let !end = fromIntegral $ bytes endDelta
  let !len = end - start
  pure
    Spanned
      { span = Span{pos = start, len}
      , value = a
      }

instance DeltaParsing m => DeltaParsing (Unspaced m) where
  line = Unspaced line
  position = Unspaced position
  slicedWith f (Unspaced m) = Unspaced $ slicedWith f m

expr :: forall m. DeltaParsing m => m (Spanned Expr)
expr =
  spanned (Lam <$ symbolic '\\' <*> ident <* symbol "->" <*> expr)
    <|> spanned (With <$ reserved "with" <*> ident <* symbolic ':' <*> typeScheme <* reserved "in" <*> expr)
    <|> spanned (IfThenElse <$ reserved "if" <*> expr <* reserved "then" <*> expr <* reserved "else" <*> expr)
    <|> app
 where
  app :: m (Spanned Expr)
  app =
    foldl
      ( \term arg ->
          Spanned
            { span = Span{pos = term.span.pos, len = (arg.span.pos - term.span.pos) + arg.span.len}
            , value = App term arg
            }
      )
      <$> atom
      <*> many atom

  atom :: m (Spanned Expr)
  atom =
    spanned (runUnspaced (Var <$> ident)) <* whiteSpace
      <|> spanned (runUnspaced (Int . fromIntegral <$> integer)) <* whiteSpace
      <|> spanned (runUnspaced (String <$> stringLiteral)) <* whiteSpace
      <|> parens expr

type_ :: (Monad m, TokenParsing m) => m Type
type_ =
  (\a -> maybe a (a `Type.Arrow`)) <$> atom <*> optional (symbol "->" *> type_)
 where
  atom =
    Type.Var <$> ident
      <|> parens type_

typeScheme :: (Monad m, TokenParsing m) => m Type.Scheme
typeScheme =
  Type.Scheme <$ reserved "forall" <*> many ident <* symbolic '.' <*> type_
    <|> Type.Scheme mempty <$> type_