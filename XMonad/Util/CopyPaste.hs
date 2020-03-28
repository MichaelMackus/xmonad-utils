module XMonad.Util.CopyPaste where

import Data.Char (isUpper)
import Data.Maybe (fromMaybe)
import System.IO (hGetContents, hPutStr, stdout)
import System.Process (readProcess, createProcess, proc, CreateProcess(..), StdStream(..))
import System.Posix.Process (forkProcess, executeFile)
import qualified Control.Exception as E

import Codec.Binary.UTF8.String (decode)

import XMonad hiding ( (|||) )
import XMonad.Util.Paste hiding (pasteSelection, pasteString)
import XMonad.Util.XSelection (getSelection)
import XMonad.Util.Ungrab

pasteSelection :: X ()
pasteSelection = getSelection >>= pasteString

pasteClipboard :: X ()
pasteClipboard = spawn "xdotool key --window $(xdotool getwindowfocus) Shift_L+Insert"

-- this isn't quite as effective as just using xdotool above
pasteClipboard' :: X ()
pasteClipboard' = getClipboard >>= pasteString

copySelection :: X ()
copySelection = spawn "xsel | xclip -selection clipboard"

-- this outputs to stderr
-- pasteString s = io $ hPutStr stdout s

pasteString s = mapM_ ( \x ->
  if x == ' ' || x == '\t' then sendKey noModMask xK_space else
  if x == '\n' || x == '\r' then sendKey noModMask xK_Return else
  --                        some reason < doesn't work with shiftMask
  if isUpper x || x `elem` "~!@#$%^&*()_+{}|:\">?" then pasteChar shiftMask x else pasteChar noModMask x
  ) s

-- this reads clipboard using xclip (created using createProcess)
-- readProcess fails since it relies on waitForProcess which fails since XMonad installs SIG_IGN for SIGCHLD
getClipboard :: X String
getClipboard = io $ do
  (_, Just hout, _, _) <-
      createProcess (proc "/usr/bin/xclip" ["-selection", "clipboard", "-o"]){ std_out = CreatePipe }
  hGetContents hout

-- similiar to getSelection from XMonad.Util.XSelection, but retrieves clipboard (doesn't depend on xclip)
getClipboard' :: MonadIO m => m String
getClipboard' = io $ do
  dpy <- openDisplay ""
  let dflt = defaultScreen dpy
  rootw  <- rootWindow dpy dflt
  win <- createSimpleWindow dpy rootw 0 0 1 1 0 0 0
  p <- internAtom dpy "CLIPBOARD" True
  ty <- E.catch
               (E.catch
                     (internAtom dpy "UTF8_STRING" False)
                     (\(E.SomeException _) -> internAtom dpy "COMPOUND_TEXT" False))
             (\(E.SomeException _) -> internAtom dpy "sTring" False)
  clp <- internAtom dpy "BLITZ_SEL_STRING" False
  xConvertSelection dpy p ty clp win currentTime
  allocaXEvent $ \e -> do
    nextEvent dpy e
    ev <- getEvent e
    if ev_event_type ev == selectionNotify
       then do res <- getWindowProperty8 dpy clp win
               return $ decode . map fromIntegral . fromMaybe [] $ res
       else destroyWindow dpy win >> return ""
