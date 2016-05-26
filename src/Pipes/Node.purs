module Pipes.Node (
    fromStream
  , fromStream'
  , toLines
  ) where

import Prelude
import Data.Tuple (Tuple(Tuple))
import Data.Monoid (mempty)
import Data.Traversable (traverse)
import Data.List(List(Cons, Nil), init, last)
import Data.Array as A
import Data.Maybe (Maybe(Just, Nothing), isJust, fromMaybe)
import Control.Bind((=<<))
import Control.Monad (when)
import Control.Monad.Trans (lift)
import Control.Monad.Aff (Aff(), launchAff, liftEff')
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Class (liftEff)
import Control.Monad.Aff.AVar (makeVar, makeVar', putVar, takeVar)
import Control.Monad.Aff.Console (log)
import Node.ChildProcess as ChildProcess
import Node.ReadLine as ReadLine
import Data.String as String
import Data.String.Regex as Regex

import Pipes
import Pipes.Core
import Pipes.Prelude hiding (show)
import Node.Stream (Readable())
import Node.Stream as Stream
import Node.Buffer as Buffer
import Node.Encoding (Encoding(UTF8))
import Node.Buffer (BUFFER, Buffer())

-- Open a read stream
_fromStream
  :: Maybe Int
  -> Readable _ _
  -> Producer_ (Maybe Buffer) (Aff _) Unit
_fromStream size r = do
  (Tuple vRequest vResponse) <- lift do
    vRequest  <- makeVar' unit
    vResponse <- makeVar
    liftEff' do
      Stream.onReadable r do
        launchAff do
          takeVar vRequest
          d <- liftEff $ Stream.read r size
          case d of
            Just buf -> putVar vResponse (Just buf)
            Nothing  -> pure unit
      Stream.onEnd r do
        launchAff do
          putVar vResponse Nothing
    pure $ Tuple vRequest vResponse
  go vRequest vResponse
  pure unit

  where
    go vReq vRes = do
      v <- lift (takeVar vRes)
      yield v
      when (isJust v) do
        lift $ putVar vReq unit
        go vReq vRes

fromStream  = _fromStream Nothing
fromStream' = _fromStream <<< pure

toLines :: Pipe (Maybe Buffer) String (Eff (buffer :: BUFFER | _)) Unit
toLines = go (Regex.regex "\r?\n" $ Regex.parseFlags "gm") ""
  where
    go rex acc = do
      mbuf <- await
      case mbuf of
          Just buf -> do
              Tuple toEmit toKeep <- lift do
                s <- Buffer.toString UTF8 buf
                let pieces = Regex.split rex $ acc <> s
                    toEmit = fromMaybe [] (A.init pieces)
                    toKeep = fromMaybe "" (A.last pieces)
                pure $ Tuple toEmit toKeep
              traverse yield toEmit
              go rex toKeep
          Nothing ->
            when (not $ String.null acc) do
              yield acc
