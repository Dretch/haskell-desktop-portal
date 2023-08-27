-- | Some helper functions to provide paths to base directories to application data\/cache\/etc.
--
-- See https://docs.flatpak.org/en/latest/conventions.html#xdg-base-directories
module Desktop.Portal.Directories
  ( getXdgConfigHome,
    getXdgDataHome,
    getXdgCacheHome,
    getXdgStateHome,
  )
where

import System.Environment (getEnv)

-- | The directory in which to store user-specific configuration files (@~\/.var\/app\/$appId\/config@ by default).
getXdgConfigHome :: IO FilePath
getXdgConfigHome = getEnv "XDG_CONFIG_HOME"

-- | The directory in which to store user-specific data (@~\/.var\/app\/$appId\/data' by default).
getXdgDataHome :: IO FilePath
getXdgDataHome = getEnv "XDG_DATA_HOME"

-- | The directory in which to store user-specific caches (@~\/.var\/app\/$appId\/cache@ by default).
getXdgCacheHome :: IO FilePath
getXdgCacheHome = getEnv "XDG_CACHE_HOME"

-- | The directory in which to store state data such as undo history (@~\/.var\/app\/$appId\/.local\/state@ by default).
getXdgStateHome :: IO FilePath
getXdgStateHome = getEnv "XDG_STATE_HOME"
