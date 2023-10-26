## 0.4.0.0
### Removed
- Remove `Desktop.Portal.Directories`. Use the directory package instead.

## 0.3.2.0
### Added
- Add Camera Portal support.

## 0.3.1.0
### Added
- Add Secret Portal support.

## 0.3.0.0
### Fixed
- The OpenURI portal now supports OpenFile and OpenDirectory correctly. This uses the file descriptor support added with the Documents portal support.

## 0.2.2.0
### Added
- Add Documents portal support.

## 0.2.1.0
### Added
- Add OpenURI portal support.

## 0.2.0.0
### Changed
- Functions now return FilePath instead of Text, where the value is always a file URI.

## 0.1.1.0
### Added
- Add Settings portal support.

## 0.1.0.0
### Added
- Make client connections explicit so they can be shared across requests and signal handlers.
- Add Notification portal support.

## 0.0.1.0

Initial release.