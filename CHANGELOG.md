
## development
- Added `calfw-toolbar-buttons` to customize buttons on the toolbar.
- Removed `calfw-default-text-sorter`, `calfw-sorter-start-time` is not set by default.
- `cfw:event` is saved as text property instead of `cfw:source` when an event is rendered.
- Added customization `calfw-fchar-period-line`.
- Removed `calfw-org-icalendars`. Pass org files directly to `calfw-org-open-calendar`.
- Added new variable `calfw-week-days-list` to include list of days that will
  be shown in the calendar.
- Changed order of arguments of `calfw-cal-create-source`,
  `calfw-ical-create-source`, `calfw-org-create-source` and
  `calfw-howm-create-source`.
- Changed and expanded optional and key arguments of
  `calfw-cal-open-diary-calendar`, `calfw-ical-open-calendar`,
  `calfw-org-open-calendar` and `calfw-howm-open-calendar`.
- Added new function `calfw--concat-wrap` which is used to wrap calendars in footer.

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
