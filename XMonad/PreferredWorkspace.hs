{-# LANGUAGE DeriveDataTypeable #-}

module XMonad.PreferredWorkspace where

import Control.Exception (Exception(..), throwIO)
import Control.Monad.IO.Class (liftIO)
import XMonad.Core
import XMonad.ManageHook
import qualified Data.List as L
import qualified XMonad.StackSet as W

data PreferredWorkspace = PreferredWorkspaces [WorkspaceId] | NonPreferred [WorkspaceId]

data PreferredWorkspaceException = PreferredWorkspaceException String deriving (Show, Typeable)

instance Exception PreferredWorkspaceException where

-- returns first workspace in preferred WS list
preferredTag :: [WorkspaceId] -> Query WorkspaceId
preferredTag p = do
    ws <- preferredTags p
    if length ws > 0 then
        return (head ws)
    else
        liftIO (throwIO (PreferredWorkspaceException "No workspaces found!"))

-- returns workspaces sorted by the preferred WS list
preferredTags :: [WorkspaceId] -> Query [WorkspaceId]
preferredTags preferred = liftX . withWindowSet $ \ws ->
    let sortf     a b  = if a `elem` preferred then LT
                         else
                          if b `elem` preferred then GT
                          else
                            if a `elem` vis then LT
                            else
                              if b `elem` vis then GT else EQ
        vis            = tag (W.current ws):tags (W.visible ws)
        tag            = W.tag . W.workspace
        tags           = map tag
    in  return (L.sortBy sortf (map W.tag (W.workspaces ws)))

