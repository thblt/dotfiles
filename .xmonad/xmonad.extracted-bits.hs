-- Bits of XMonad configuration I may need again

-- A MultiToggle transformer for decorations
data DecoTransformer = DECO deriving (Read, Show, Eq, Typeable)
instance Transformer DecoTransformer Window where
  transform _ x k = k (myDecoration x) (const x)

-{- DBus interface -}

import qualified DBus
import qualified DBus.Client                         as DBus

getWellKnownName :: DBus.Client -> IO ()
getWellKnownName dbus = do
  DBus.requestName dbus (DBus.busName_ "org.xmonad.Log")
                [DBus.nameAllowReplacement, DBus.nameReplaceExisting, DBus.nameDoNotQueue]
  return ()

dbusLogger :: DBus.Client -> X ()
dbusLogger dbus = do

  {- Note to future self: Would you need to get Xinerama to work (report
     screen for each workspace) look at the code of
     XMonad.Hooks.DynamicLog.pprWindowSetXinerama -}

  winset <- gets windowset
  sortws <- getSortByIndex
--  urgents <- readUrgents

  -- layout description
  let ld = description . XSS.layout . XSS.workspace . XSS.current $ winset

      -- workspace list
      all_ws = sortws $ XSS.workspaces winset
      -- this workspace
      this_ws = XSS.currentTag winset
      -- all visible
      vis_ws = fmap (XSS.tag . XSS.workspace) (XSS.visible winset)

      workspaces = fmap (\ws -> let id = getWsName ws in
                                  (id, -- Name
                                   0:: Int32, -- screen id (xinerama, unimplemented)
                                   id == this_ws, -- is current
                                   id `elem` vis_ws, -- is visible but not current (xinerama, untested)
                                   isJust (XSS.stack ws), -- contains windows
                                   id `elem` myHiddenWorkspaces) -- is hidden
                        ) all_ws


  -- window title
  title <- maybe (return "") (fmap show . getName) . XSS.peek $ winset

  let signal = (DBus.signal (DBus.objectPath_ "/org/xmonad/Log") (DBus.interfaceName_ "org.xmonad.Log") (DBus.memberName_ "Update")) {
        DBus.signalBody = [
            DBus.toVariant ld,
            DBus.toVariant workspaces,
            DBus.toVariant title
            ]
        }
  liftIO $ DBus.emit dbus signal
    where
      getWsName (XSS.Workspace t _ _) = t
