module Test.Main where

import Prelude
import Global (readInt)
import Data.Int (odd)
import Data.Int as Int
import Data.List as List
import Data.Maybe.Unsafe (fromJust)
import Control.Monad.Trans (lift)
import Control.Monad.Aff (Aff(), launchAff, later')
import Control.Monad.Eff (Eff())
import Control.Monad.Eff.Class (liftEff)
import Control.Monad.Eff.Class (class MonadEff)
import Control.Monad.Aff.Console (log)
import Control.Monad.Morph
import Node.FS.Stream as FS
import Pipes (for, (>->), yield, await)
import Pipes.Core (runEffect)
import Pipes.Prelude (take, chain, filter, map, toListM)
import Pipes.Node
import Node.ChildProcess as ChildProcess
import Node.ChildProcess (ChildProcess(), CHILD_PROCESS)
import Data.Posix.Signal (Signal(..))
import Test.Unit (suite, test)
import Test.Unit.Main (runTest)
import Test.Unit.Assert as Assert

main = runTest do
  suite "process streams" do
    test "reads stdout lazily" do
      proc <- liftEff do
        ChildProcess.spawn
          "seq"
          ["1000000"]
          ChildProcess.defaultSpawnOptions
      xs <- toListM (fromStream (ChildProcess.stdout proc)
                          >-> (liftEff =<| toLines)
                          >-> (map $ fromJust <<< Int.fromString)
                          >-> (filter odd)
                          >-> (chain (\_ -> later' 10 (pure unit)))
                          >-> (take 10)
                          >-> (map (_ * 10))
                      )
      liftEff $ ChildProcess.kill SIGKILL proc
      Assert.equal [10, 30, 50, 70, 90, 110, 130, 150, 170, 190] (List.fromList xs)
