module XMonad.Util.Focus (focusedWin) where

import XMonad hiding ( (|||) )


-- return active window using root window _NET_ACTIVE_WINDOW property
focusedWin :: X (Maybe Window)
focusedWin = do
    dpy   <- asks display
    let dflt = defaultScreen dpy

    io $ do
        rootw  <- rootWindow dpy dflt
        atom   <- internAtom dpy "_NET_ACTIVE_WINDOW" False
        prop   <- rawGetWindowProperty 32 dpy atom rootw

        case prop of
            -- there should only be one active window
            Just (p:ps) -> return (Just p)
            otherwise   -> return Nothing

