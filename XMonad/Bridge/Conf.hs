module XMonad.Bridge.Conf (mkConfig, defaultConfig) where

import GHC.IO.Handle.Types (Handle)
import XMonad.Core
import XMonad.Layout.Fullscreen (fullscreenEventHook)
import XMonad.Util.Run (spawnPipe)
import qualified XMonad.Config as Conf

type Executable = String

-- mkconfig from config loghook & executable to pass to loghook
mkConfig :: XConfig a -> (Handle -> X ()) -> Executable -> IO (XConfig a)
mkConfig conf logh exe = do
    logh' <- fmap logh (spawnPipe exe)
    return (conf { logHook = logh' })

defaultConfig = Conf.defaultConfig {
                  -- hooks
                  terminal = "urxvt",
                  handleEventHook = fullscreenEventHook,
                  -- misc
                  focusFollowsMouse = False,
                  clickJustFocuses = False
                }
