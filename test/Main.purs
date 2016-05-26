module Test.Main where

import Prelude
import Control.Monad.Trans (lift)
import Control.Monad.Aff (Aff(), launchAff)
import Control.Monad.Eff (Eff())
import Control.Monad.Eff.Class (liftEff)
import Control.Monad.Eff.Class (class MonadEff)
import Control.Monad.Aff.Console (log)
import Control.Monad.Morph
import Node.FS.Stream as FS
import Pipes (for, (>->))
import Pipes.Core (runEffect)
import Pipes.Prelude (take)
import Pipes.Node

main = launchAff do
  s <- liftEff $ FS.createReadStream "/Users/felix/data"
  runEffect $ for (fromStream s
                      >-> (liftEff =<| toLines)
                      >-> (take 1)
                  )
                  (lift <<< log)
  log "yay"

