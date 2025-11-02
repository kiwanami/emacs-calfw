# Calfw - A calendar framework for Emacs

## What is calfw?

This program displays a calendar view in the Emacs buffer.

![Calfw image](https://cacoo.com/diagrams/OnjKgBHat0kHs0xp-9E5E0.png?width=600)

### Screenshots

Currently, calfw has 4 views, month, 1week, 2week and day view.
![Views](https://cacoo.com/diagrams/OnjKgBHat0kHs0xp-F3756.png?width=600)

Pushing SPC key, the detail buffer pops up. Pushing SPC key again, the buffer is closed.
![Pop up details](https://cacoo.com/diagrams/OnjKgBHat0kHs0xp-83C80.png?width=600)

Many information items are displayed in the Emacs buffer.
![View details](https://cacoo.com/diagrams/OnjKgBHat0kHs0xp-B961B.png?width=600)

## Installation

To use this program, locate this file to load-path directory,
and add the following code to your .emacs.

```el
(require 'calfw)

;; For compatibility with the old name scheme (starting with `cfw:`
;; instead of `calfw-`), uncomment the following code:
;;
;; (setq calfw-compat-mark-obsolete nil) ;; This suppresses obsolete messages
;; (require 'calfw-compat)
```

Executing the command `calfw-open-calendar-buffer`, switch to the calendar buffer.
You can navigate the date like calendar.el.

Schedule data which are shown in the calendar view, are collected by a
list of the struct `calfw-source` objects through the named argument
variables `:contents-sources` and `:annotation-sources`. The former
source defines schedule contents. The later one does date
annotations like the moon phases.

This program gets the holidays using the function
`calendar-holiday-list`. See the document for the holidays.el and the Info text.

## Key bindings

In the calendar buffer and region, you can use following key bindings:

| Navigation          |                                              |
|---------------------|----------------------------------------------|
| [left], b, h        | Previous day                                 |
| [right], f, l       | Next day                                     |
| [up], p, k          | Previous week                                |
| [down], n, j        | Next week                                    |
| ^                   | Week begin                                   |
| $                   | Week end                                     |
| [home]              | First date in this month                     |
| [end]               | Last date in this month                      |
| [pageup]            | Previous month                               |
| [pagedown]          | Next month                                   |
| &lt;                | Move current view backward                   |
| &gt;                | Move current view forward                    |
| t                   | Today                                        |
| g                   | Absolute date (YYYY/MM/DD)                   |
| TAB                 | Next item in a day                           |

| Changing View       |                                              |
|---------------------|----------------------------------------------|
| M                   | Month view                                   |
| W                   | 1 Week view                                  |
| T                   | 2 Week view                                  |
| D                   | Day view                                     |

| Operation           |                                              |
|---------------------|----------------------------------------------|
| r                   | Refresh data and re-draw contents            |
| SPC                 | Pop-up detail buffer (like Quicklook in Mac) |
| RET, [click]        | Jump (howm, orgmode)                         |
| q                   | Bury buffer                                  |

The buttons on the toolbar can be clicked.

## Add-ons:

Following programs are also useful:

- calfw-howm.el : Display howm schedules (http://howm.sourceforge.jp/index.html)
- calfw-ical.el : Display schedules of the iCalendar format, such as the google calendar.
- calfw-org.el  : Display org schedules (http://orgmode.org/)
- calfw-cal.el  : Display diary schedules.

## Setting example:

### For howm users:

```el
(eval-after-load "howm-menu" '(progn
  (require 'calfw-howm)
  (calfw-howm-install-schedules)
  (define-key howm-mode-map (kbd "M-C") 'calfw-howm-open-calendar)))
```

If you are using Elscreen, here is useful.

```el
(define-key howm-mode-map (kbd "M-C") 'calfw-howm-elscreen-open-calendar)
```

You can display a calendar in your howm menu file.

```
%here%(calfw-howm-schedule-inline)
```

![howm menu embedding](https://cacoo.com/diagrams/vrScI4K2QlmDApfd-1F941.png?width=450)

### For org users:

    (require 'calfw-org)

Then, M-x `calfw-org-open-calendar`.

![org-agenda and calfw-org](https://cacoo.com/diagrams/S6aJntG6giGs44Yn-89CB2.png?width=450)

#### Filtering agenda items

You can choose agenda items with `calfw-org-agenda-schedule-args`, like following code:

```el
(setq calfw-org-agenda-schedule-args '(:timestamp))
```

This setting restricts items containing a date stamp or date range matching the selected date.
If `calfw-org-agenda-schedule-args` is `nil`, the default custom variable `org-agenda-entry-types` is used. For the further information, please refer the orgmode document.

- [Worg: Speeding up custom agenda commands](http://orgmode.org/worg/org-tutorials/org-custom-agenda-commands.html#sec-5)

#### Orgmode like key binding

You can use another key binding like org agenda buffer, setting `calfw-org-overwrite-default-keybinding` to non-nil, like following code:

```el
(setq calfw-org-overwrite-default-keybinding t)
```

Then, following key bindings are overwritten:

| key   | function
|-------|----------------------------------------
|   g   | Refresh data and re-draw contents (calfw-refresh-calendar-buffer)
|   j   | Goto the specified date (calfw-org-goto-date)
|   k   | org-capture
|   x   | Close calfw and other buffers opened by calfw-org (calfw-org-clean-exit)
|   d   | Day view (calfw-change-view-day)
|   v d | Day view (calfw-change-view-day)
|   v w | 1 Week view (calfw-change-view-week)
|   v m | Month View (calfw-change-view-month)

#### Synchronization with google calendar

Here is the program which helps synchronization schedule items between org and google calendar, and also collaborates with calfw.

- https://github.com/myuhe/org-gcal.el
    - [Org-modeとGoogle calendar間で予定をやりとりするorg-gcal.elというのを作りました](http://sheephead.homelinux.org/2014/03/14/7023/)
    - [calfwとorg-gcalの連携](http://sheephead.homelinux.org/2014/03/15/7035/)

### For iCal (Google Calendar) users:

Here is a minimum sample code:

```el
(require 'calfw-ical)
(calfw-ical-open-calendar "http://www.google.com/calendar/ical/.../basic.ics")
```

![Google Calendar and calfw-ical](https://cacoo.com/diagrams/vrScI4K2QlmDApfd-5E808.png?width=450)

Here is the add-on program which communicate with google calendar via API:

- [calfwからGoogleカレンダーを編集するcalfw-gcal.elを書いてみた](http://sheephead.homelinux.org/2011/01/18/6559/)
    - https://github.com/myuhe/calfw-gcal.el/blob/master/calfw-gcal.el

### For diary users:

Here is a minimum sample code:

```el
(require 'calfw-cal)
```

Then, M-x `calfw-cal-open-diary-calendar`.

If you see a blank entry for each day, set the variable `diary-list-include-blanks` to nil.

### General setting

The calfw view can display many schedule items, gathering some schedule sources.
Using the function `calfw-open-calendar-buffer` is the general way to display the schedules.

Here is the sample code:

```el
(require 'calfw-cal)
(require 'calfw-ical)
(require 'calfw-howm)
(require 'calfw-org)

(defun my-open-calendar ()
  (interactive)
  (calfw-open-calendar-buffer
   :contents-sources
   (list
    (calfw-org-create-source nil "org-agenda" "Green")  ; orgmode source from org-agenda files
    (calfw-howm-create-source "howm" "Blue")  ; howm source
    (calfw-cal-create-source "diary" "Orange") ; diary source
    (calfw-ical-create-source "~/moon.ics" "Moon" "Gray")  ; ICS source1
    (calfw-ical-create-source "https://..../basic.ics" "gcal" "IndianRed") ; google calendar ICS
   )))
```

The function `calfw-open-calendar-buffer` receives schedules sources via
the named argument `:contents-sources`.

One can customize the keymap on the calendar buffer with the named
argument `:custom-map` of `calfw-open-calendar-buffer`.


### Grid frame

Users can have nice unicode grid frame, if your emacsen supports it. For example:

```el
;; Default setting
(setq calfw-fchar-junction ?+
      calfw-fchar-vertical-line ?|
      calfw-fchar-horizontal-line ?-
      calfw-fchar-left-junction ?+
      calfw-fchar-right-junction ?+
      calfw-fchar-top-junction ?+
      calfw-fchar-top-left-corner ?+
      calfw-fchar-top-right-corner ?+ )

;; Unicode characters
(setq calfw-fchar-junction ?╋
      calfw-fchar-vertical-line ?┃
      calfw-fchar-horizontal-line ?━
      calfw-fchar-left-junction ?┣
      calfw-fchar-right-junction ?┫
      calfw-fchar-top-junction ?┯
      calfw-fchar-top-left-corner ?┏
      calfw-fchar-top-right-corner ?┓)

;; Another unicode chars
(setq calfw-fchar-junction ?╬
      calfw-fchar-vertical-line ?║
      calfw-fchar-horizontal-line ?═
      calfw-fchar-left-junction ?╠
      calfw-fchar-right-junction ?╣
      calfw-fchar-top-junction ?╦
      calfw-fchar-top-left-corner ?╔
      calfw-fchar-top-right-corner ?╗)
```

### Line breaking

If a content string is longer than the cell width, the calfw breaks into the multiple lines.
In the current implementation, the Calfw has 3 strategies: none, simple and wordwrap. The variable `calfw-render-line-breaker` selects the strategy to break lines.

- `calfw-render-line-breaker-none`
    - Never breaks lines. Longer contents are truncated.
- `calfw-render-line-breaker-simple` (default)
    - This strategy breaks lines with rigid width. This may be not so beautiful, but In the most cases it looks good.
- `calfw-render-line-breaker-wordwrap`
    - This strategy breaks lines with the emacs function `fill-region`. Although, the line breaking algorithm of the Emacs is not so smart as more complicated ones, such as Knuth/Plass algorithm, this strategy is better than the simple one.


## Calfw framework details

In this section, I would explain how to add a new calendar source and how to embed the calfw component in the other applications.

### How to add a new calendar source?

Defining the `calfw-source` object, one can extend calfw calendar source.

#### struct calfw-source details

The struct `calfw-source` is a simple data type defined by cl-defstruct.

Here is the details of the slot members of `calfw-source`.

| slot name       | description                                                                                                                        |
|-----------------|------------------------------------------------------------------------------------------------------------------------------------ |
| name            | [required] Source name. This name is shown at the status bar.                                                                      |
| data            | [required] Data function which returns calendar contents. The function details are described in the next section.                  |
| update          | [option] Update function. Calfw calls this function when this source needs to refresh the data.                                    |
| color           | [option] Color string for this source.  Color names those are shown by `M-x list-colors-display` or RGB hex format like "#abcdef". |
| period-fgcolor  | [option] Foreground color for period items. The default color is white or black.                                                   |
| period-bgcolor  | [option] Background color for period items. The default color is `calfw-source-color`.                                               |
| opt-face        | [option] Additional options for the normal item face.  Ex. `:opt-face '(:weight bold)`                                             |
| opt-period-face | [option] Additional options for the period item face.                                                                              |

Only `name` and `data` slots are essential. Many slots are visual options.

In many cases, one has to specify only the `color` slot for visual,
because the calfw chooses appropriate colors for the rest color options.

#### calfw-source-data details

This section explains what objects the function-slot `calfw-source-data` should return.

The function-slot `calfw-source-data` receives two arguments, start and
end date of the query period, and returns a list of instances of `calfw-event` struct.

Here is a simple example.

`calfw-source-data example1:`

```el
;; calfw-source-data example
(defun sample-data1 (b e)
  (list
    (make-calfw-event :title "item1"   :start-date  (calfw-date 1 1 2011))
    (make-calfw-event :title "item2-1" :start-date  (calfw-date 1 10 2011))
    (make-calfw-event :title "item2-2" :start-date  (calfw-date 1 10 2011))))

(calfw-open-calendar-buffer
  :date (calfw-date 1 1 2011)
  :contents-sources
   (list
     (make-calfw-source
      :name "test1" :data 'sample-data1)))
```

Evaluating this code in the scratch buffer, following result is displayed.

![Simple source example](https://cacoo.com/diagrams/P6baUrxEQj4NYheV-50310.png?width=450)

The date is specified by `calfw-date` type, `([month] [day] [year])`. This format is commonly used in calendar.el and orgmode.
(I diagrammed the exchange ways for some time and date formats in Emacs, [here](https://cacoo.com/diagrams/lsA64PTazlLTbSwR).)

Period items are little different. One period item is specified by
`:start-date` and `:end-date`, and the nested list which has the symbol `periods` at the head collects them, like the following code.

`calfw-source-data example2:`

```el
;; calfw-source-data period items
(defun sample-data2 (b e)
  (list
    (make-calfw-event :title "Item1"
          :start-date  (calfw-date 1 15 2011))
    (list 'periods
      (make-calfw-event :title "Period item"
          :start-date (calfw-date 1 8 2011)
          :end-date   (calfw-date 1 9 2011)
          :description "Period item description")
      (make-calfw-event :title "Next item"
          :start-date (calfw-date 1 11 2011)
          :end-date   (calfw-date 1 12 2011)
          :description "Next item description"))))

(calfw-open-calendar-buffer
  :date (calfw-date 1 1 2011)
  :contents-sources
   (list
     (make-calfw-source
      :name "test2" :data 'sample-data2)))
```

Evaluating this code in the scratch buffer, following result is displayed.

![Range items example](https://cacoo.com/diagrams/P6baUrxEQj4NYheV-40315.png?width=450)

Here are other detailed specifications.

- The both start and end date are included by the query period.
- The items those aren't included in the query period are ignored.
- `calfw-source-data` should return a value as fast as possible, because users are waiting for the result. Caching is good idea.
- Schedule items don't have to be ordered. Duplicated items may be gathered.
- In the day cell, the items are sorted by `string-lessp`, i.e. numerical and alphabetical order.
  - The ordering function can be customized by the named argument `:sorter` of the component construction.

In the above examples, the dates of the schedule items are fixed. The actual sources generate result values by the programs. The codes of calfw add-ons may be helpful for your implementation.

##### calfw-event struct detail

The `calfw-event` struct:

| slot name     | description                                 |
|---------------|---------------------------------------------|
| `title`       | event title [string]                        |
| `start-date`  | start date of the event [calfw-date]          |
| `start-time`  | start time of the event (optional)          |
| `end-date`    | end date of the event [calfw-date] (optional) |
| `end-time`    | end of the event (optional)                 |
| `description` | event description [string] (optional)       |
| `location`    | location [string] (optional)                |
| `source`      | [internal] source of the event              |

##### Event formatting

The framework has several formatting functions for `calfw-event` instances.
The functions are used by the calfw plugins (cal,ical, etc) to display in a common way.

| Format function             | Description                                                 |
|-----------------------------|-------------------------------------------------------------|
| `calfw-event-overview`        | To get an overview of the event (month, 2-week & week view) |
| `calfw-event-days-overview`   | Overview in day-view.                                       |
| `calfw-event-period-overview` | Overview of periods (same for all views)                    |
| `calfw-event-detail`          | Detailed information of the event for the detail-view       |

The formatting can be customized by the user with several formatting strings:

- `calfw-event-format-overview`
- `calfw-event-format-days-overview`
- `calfw-event-format-period-overview`
- `calfw-event-format-detail`
- `calfw-event-format-title`
- `calfw-event-format-start-date`
- `calfw-event-format-start-time`
- `calfw-event-format-end-date`
- `calfw-event-format-end-time`
- `calfw-event-format-location`
- `calfw-event-format-description`

#### Examples

- [calfw-git.el](https://gist.github.com/kiwanami/d77d9669440f3336bb9d)
    - Displaying git commit history items in calfw calendar view
- [calfw-syobocal.el](https://gist.github.com/kiwanami/1fd257fc1e8907d4d92e)
    - Retrieving schedule items via Web API and displaying them in calfw calendar view

### How to embed the calfw component in the other applications?

In this section, the details of calfw components would be explained so as for users to extend calfw in themselves.

Calfw is built on the MVC architecture, using simple structure objects and modules employed by naming rules.

#### Calfw component

Calfw has three destination components to display the calendar.

- Independent buffer
- Region in the other buffer
- Text output

##### Buffer

The 'buffer' destination displays the calendar view as ordinary Emacs applications do.

The function `calfw-open-calendar-buffer` makes a new calendar buffer (calfw buffer) and displays it by `switch-to-buffer`. The major mode of the calfw buffer is `calfw-calendar-mode` and the keymap `calfw-calendar-mode-map` is bound.

This destination is easy to use for applications and users, because the buffer is usual application boundary and users know how to use buffers.

##### Region

The 'Region' destination embeds the calendar view in the buffer which is managed by the other applications. This destination can give the other applications a nice calendar view. See the howm embedding for example.

Let's try a demonstration. Evaluate this code in your scratch buffer.

Region destination example:

```el
;; Evaluate this code in the scratch buffer
(require 'calfw)
(calfw-create-calendar-component-region :height 10)
```

Then, the calendar view will be embedded in the scratch buffer like the following screenshot. You can navigate the calfw view in the buffer. Undoing for the some times, you can remove the calfw view.

![calfw in the scratch buffer](https://cacoo.com/diagrams/P6baUrxEQj4NYheV-B9649.png?width=600)

Because this destination never interacts anything out of the region and has its own key-binds as a text property, users can easily embed a calendar view in the other applications.

##### Text

The 'text' destination generates just a text which represent calfw view. The function `calfw-get-calendar-text` returns the text.

##### Destination and View

Three destinations are explained as mentioned above. Although they have different appearance, the application can operate the calfw component in the same way.

Let us call them 'destination', it is the abstraction of UI components.

The similar word 'view' means in which form the calfw displays the contents, for example, monthly form, two-weeks and weekly one and etc.
