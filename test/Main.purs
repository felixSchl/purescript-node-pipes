module Test.Main where

import Prelude
import Control.Monad.Trans (lift)
import Control.Monad.Aff (launchAff)
import Control.Monad.Eff.Class (liftEff)
import Control.Monad.Aff.Console (log)
import Node.FS.Stream as FS
import Pipes (for, (>->))
import Pipes.Core (runEffect)
import Pipes.Node

main = launchAff do
  s <- liftEff $ FS.createReadStream "/Users/felix/data"
  runEffect $ for (fromStream s >-> lines) (lift <<< log)
  log "yay"

