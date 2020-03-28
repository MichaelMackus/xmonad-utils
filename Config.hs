module Config (Config(..)) where

import XMonad.Core (ScreenId, WorkspaceId, WindowSet)
import XMonad.Config (defaultConfig)
import qualified XMonad.Core as X

type Executable = String
type Tag = String

data Config = Config {
  terminal :: Executable,
  music    :: Executable,
  workspaces :: [WorkspaceId],
  preferredScreens :: [(Tag, ScreenId)],
  viewf :: Tag -> WindowSet -> WindowSet
}

-- mkXConfig :: Config -> X.XConfig l
mkXConfig conf = defaultConfig {
    X.terminal = terminal conf,
    X.workspaces = workspaces conf
}
