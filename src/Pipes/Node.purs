module Pipes.Node (
    fromStream
  , fromStream'
  , lines
  ) where

import Prelude
import Data.Tuple (Tuple(Tuple))
import Data.Monoid (mempty)
import Data.Maybe (Maybe(Just, Nothing))
import Control.Bind((=<<))
import Control.Monad.Trans (lift)
import Control.Monad.Aff (Aff(), launchAff, liftEff')
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Class (liftEff)
import Control.Monad.Aff.AVar (makeVar, makeVar', putVar, takeVar)
import Control.Monad.Aff.Console (log)
import Node.ChildProcess as ChildProcess
import Node.ReadLine as ReadLine

import Pipes
import Pipes.Core
import Pipes.Prelude hiding (show)
import Node.Stream (Readable())
import Node.Stream as Stream
import Node.Buffer as Buffer
import Node.Encoding (Encoding(UTF8))
import Node.Buffer (BUFFER, Buffer())

-- Open a read stream
_fromStream :: Maybe Int -> Readable _ _ -> Producer_ Buffer (Aff _) Unit
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
            Just buf -> putVar vResponse buf
            Nothing  -> pure unit
    pure $ Tuple vRequest vResponse
  go vRequest vResponse
  pure unit

  where
    go vReq vRes = do
      yield =<< lift (takeVar vRes)
      lift $ putVar vReq unit
      go vReq vRes

fromStream  = _fromStream Nothing
fromStream' = _fromStream <<< pure

lines :: Pipe Buffer String (Aff (buffer :: BUFFER | _)) Unit
lines = do
  buf <- await
  s   <- lift do
          liftEff do
            Buffer.toString UTF8 buf
  yield s
  return unit
