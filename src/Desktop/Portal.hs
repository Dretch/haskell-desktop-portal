-- |
-- Wrappers around the XDG Desktop Portal D-BUS API.
--
-- See the documentation for the underlying API: https://flatpak.github.io/xdg-desktop-portal
module Desktop.Portal
  ( -- * Connection Management
    Internal.Client,
    Internal.connect,
    Internal.disconnect,
    Internal.clientName,

    -- * Request Management
    Internal.Request,
    Internal.await,
    Internal.cancel,

    -- * Signal Management
    Internal.SignalHandler,
    Internal.cancelSignalHandler,

    -- * Portal Interfaces
    module Desktop.Portal.Account,
    module Desktop.Portal.Directories,
    module Desktop.Portal.FileChooser,
    module Desktop.Portal.Notification,
    module Desktop.Portal.OpenURI,
  )
where

import Desktop.Portal.Account
import Desktop.Portal.Directories
import Desktop.Portal.FileChooser
import Desktop.Portal.Internal qualified as Internal
import Desktop.Portal.Notification
import Desktop.Portal.OpenURI hiding (OpenFileOptions (..), openFile, openFileOptions) -- avoid conflict with FileChooser.openFile
