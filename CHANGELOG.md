
## development
- Added `calfw-toolbar-buttons` to customize buttons on the toolbar.
- Removed `calfw-default-text-sorter`, `calfw-sorter-start-time` is not set by default.
- `cfw:event` is saved as text property instead of `cfw:source` when an event is rendered.
- Added customization `calfw-fchar-period-line`.
- Removed `calfw-org-icalendars`. Pass org files directly to
  `calfw-org-open-calendar`.

## 2.0
- Renamed all symbols to start with the package name.
- Added `calfw-compat` for compatibility with old code.
- Fixed documentation to adhere to checkdoc guidelines.
- Removed `[internal]` crumbs, using `--` in the function name instead.

## 1.7

This fork has the following change
- Applied three external pull requests (See Pull requests for more info)
- Implement a `noerror` mode for `cfw:cp-get-component`
- Showing calendars on separate lines and allowing showing/hiding of them.
- Removed everything to do with selecting a date, preferring instead to use
  the point to indicate selection.
- Changed colouring of background/foreground.
- Removed M-v and C-v binding.
- Cleaned up some of the code and using more standard functions, though much
  more can be done (WIP).
