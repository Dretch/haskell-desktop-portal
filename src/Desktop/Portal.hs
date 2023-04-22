-- |
-- Wrappers around the XDG Desktop Portal D-BUS API.
--
-- See the documentation for the underlying API: https://flatpak.github.io/xdg-desktop-portal
module Desktop.Portal
  ( module Desktop.Portal.Account,
    module Desktop.Portal.FileChooser,
    module Desktop.Portal.Request,
  )
where

import Desktop.Portal.Account
import Desktop.Portal.FileChooser
import Desktop.Portal.Request
