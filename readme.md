# Calfw - A calendar framework for Emacs

## What is calfw?

This program displays a calendar view in the Emacs buffer.

![Calfw image](https://cacoo.com/diagrams/OnjKgBHat0kHs0xp-9E5E0.png?width=600)

### Screenshots

Currently, calfw has 4 views, month, 1week, 2week and day view.
![Views](https://cacoo.com/diagrams/OnjKgBHat0kHs0xp-F3756.png?width=600)

Pushing SPC key, the detail buffer pops up. Pushing SPC key again, the buffer is closed.
![Pop up details](https://cacoo.com/diagrams/OnjKgBHat0kHs0xp-83C80.png?width=600)

Many informations are displayed in the Emacs buffer.
![View details](https://cacoo.com/diagrams/OnjKgBHat0kHs0xp-B961B.png?width=600)

## Installation

To use this program, locate this file to load-path directory,
and add the following code to your .emacs.

    (require 'calfw)

Executing the command `cfw:open-calendar-buffer', switch to the calendar buffer.
You can navigate the date like calendar.el.

Schedule data which are shown in the calendar view, are collected by a
list of the struct `cfw:source` objects through the named argument
variables `:contents-sources` and `:annotation-sources`. The former
source defines schedule contents. The later one does date
annotations like the moon phases.

This program gets the holidays using the function
`calendar-holiday-list`. See the document for the holidays.el and the Info text.

## Key bindings

In the calendar buffer and region, you can use following key bindings:

<table>
  <tr><th>Navigation             </th><th></th></tr>
  <tr><td>  [left], b, h         </td><td> Previous day</td></tr>
  <tr><td>  [right], f, l        </td><td> Next day</td></tr>
  <tr><td>  [up], p, k           </td><td> Previous week</td></tr>
  <tr><td>  [down], n, j         </td><td> Next week</td></tr>
  <tr><td>  ^                    </td><td> Week begin</td></tr>
  <tr><td>  $                    </td><td> Week end</td></tr>
  <tr><td>  [home]               </td><td> First date in this month</td></tr>
  <tr><td>  [end]                </td><td> Last date in this month</td></tr>
  <tr><td>  M-v, [PgUp], &lt;    </td><td> Previous month</td></tr>
  <tr><td>  C-v, [PgDown], &gt;  </td><td> Next month</td></tr>
  <tr><td>  t                    </td><td> Today</td></tr>
  <tr><td>  g                    </td><td> Absolute date (YYYY/MM/DD)</td></tr>
  <tr><th>Changing View          </th><th></th></tr>
  <tr><td>  M                    </td><td> Month view</td></tr>
  <tr><td>  W                    </td><td> 1 Week view</td></tr>
  <tr><td>  T                    </td><td> 2 Week view</td></tr>
  <tr><td>  D                    </td><td> Day view</td></tr>
  <tr><th>Operation              </th><th></th></tr>
  <tr><td>  r                    </td><td> Refresh data and re-draw contents</td></tr>
  <tr><td>  SPC                  </td><td> Pop-up detail buffer (like Quicklook in Mac)</td></tr>
  <tr><td>  RET, [click]         </td><td> Jump (howm, orgmode)</td></tr>
  <tr><td>  q                    </td><td> Bury buffer</td></tr>
</table>

The buttons on the toolbar can be clicked.

## Add-ons:

Following programs are also useful:

- calfw-howm.el : Display howm schedules (http://howm.sourceforge.jp/index.html)
- calfw-ical.el : Display schedules of the iCalendar format, such as the google calendar.
- calfw-org.el  : Display org schedules (http://orgmode.org/)
- calfw-cal.el  : Display diary schedules.

## Setting example:

### For howm users:

    (eval-after-load "howm-menu" '(progn
      (require 'calfw-howm)
      (cfw:install-howm-schedules)
      (define-key howm-mode-map (kbd "M-C") 'cfw:open-howm-calendar)
    ))


If you are using Elscreen, here is useful.

    (define-key howm-mode-map (kbd "M-C") 'cfw:elscreen-open-howm-calendar)

You can display a calendar in your howm menu file.

    %here%(cfw:howm-schedule-inline)

![howm menu embedding](https://cacoo.com/diagrams/vrScI4K2QlmDApfd-1F941.png)

### For org users:

    (require 'calfw-org)

Then, M-x cfw:open-org-calendar.

![org-agenda and calfw-org](https://cacoo.com/diagrams/S6aJntG6giGs44Yn-89CB2.png)

### For iCal (Google Calendar) users:

Here is a minimum sample code:


    (require 'calfw-ical)
    (cfw:open-ical-calendar "http://www.google.com/calendar/ical/.../basic.ics")

![Google Calendar and calfw-ical](https://cacoo.com/diagrams/vrScI4K2QlmDApfd-5E808.png)

### For diary users:

Here is a minimum sample code:

    (require 'calfw-cal)

Then, M-x cfw:open-diary-calendar.

### General setting

The calfw view can display many schedule items, gathering some schedule sources.
Using the function `cfw:open-calendar-buffer` is the general way to display the schedules.

Here is the sample code:

    (require 'calfw-cal)
    (require 'calfw-ical)
    (require 'calfw-howm)
    (require 'calfw-org)
    
    (defun my-open-calendar ()
      (interactive)
      (cfw:open-calendar-buffer
       :contents-sources
       (list
        (cfw:org-create-source "Green")  ; orgmode source
        (cfw:howm-create-source "Blue")  ; howm source
        (cfw:cal-create-source "Orange") ; diary source
        (cfw:ical-create-source "Moon" "~/moon.ics" "Gray")  ; ICS source1
        (cfw:ical-create-source "gcal" "https://..../basic.ics" "IndianRed") ; google calendar ICS
       ))) 

The function `cfw:open-calendar-buffer` receives schedules sources via
the named argument `:contents-sources`.

One can customize the keymap on the calendar buffer with the named
argument `:custom-map` of `cfw:open-calendar-buffer`.


## Customize

### Holidays

The calfw collects holidays from the customize variable
`calendar-holidays` which belongs to holidays.el in the Emacs. See the
document and source of holidays.el for details.

### Format of month and week days

The calfw uses some customization variables in the calendar.el.

Here is a customization code:

    ;; Month
    (setq calendar-month-name-array
      ["January" "February" "March"     "April"   "May"      "June"
       "July"    "August"   "September" "October" "November" "December"])
    
    ;; Week days
    (setq calendar-day-name-array
          ["Sunday" "Monday" "Tuesday" "Wednesday" "Thursday" "Friday" "Saturday"])
    
    ;; First day of the week
    (setq calendar-week-start-day 0) ; 0:Sunday, 1:Monday

### Faces

TODO...

### Grid frame

Users can have nice unicode grid frame. However, in the some environment, the Emacs can not display the grid characters correctly. Please try following settings.

Grid setting example:

    ;; Default setting
    (setq cfw:fchar-junction ?+
          cfw:fchar-vertical-line ?|
          cfw:fchar-horizontal-line ?-
          cfw:fchar-left-junction ?+
          cfw:fchar-right-junction ?+
          cfw:fchar-top-junction ?+
          cfw:fchar-top-left-corner ?+
          cfw:fchar-top-right-corner ?+ )
    
    ;; Unicode characters
    (setq cfw:fchar-junction ?╋
          cfw:fchar-vertical-line ?┃
          cfw:fchar-horizontal-line ?━
          cfw:fchar-left-junction ?┣
          cfw:fchar-right-junction ?┫
          cfw:fchar-top-junction ?┯
          cfw:fchar-top-left-corner ?┏
          cfw:fchar-top-right-corner ?┓)
          
    ;; Another unicode chars
    (setq cfw:fchar-junction ?╬
          cfw:fchar-vertical-line ?║
          cfw:fchar-horizontal-line ?═
          cfw:fchar-left-junction ?╠
          cfw:fchar-right-junction ?╣
          cfw:fchar-top-junction ?╦
          cfw:fchar-top-left-corner ?╔
          cfw:fchar-top-right-corner ?╗)

## Calfw framework details

In this section, I would explain how to add a new calendar source and how to embed the calfw component in the other applications.

### How to add a new calendar source?

Defining the `cfw:source` object, one can extend calfw calendar source.

#### struct 'cfw:source' details

The struct `cfw:source` is a simple data type defined by cl-defstruct.

Here is the details of the slot members of cfw:source.

<table>
<tr><th> slot name       </th><th> 
   description </th></tr>
<tr><td> name            </td><td>
   [required] Source name. This name is shown at the status bar.
</td></tr>
<tr><td> data            </td><td>
   [required] Data function which returns calendar contents.
   The function details are described in the next section.
</td></tr>
<tr><td> update          </td><td>
   [option] Update function. Calfw calls this function when this source needs to refresh the data.
</td></tr>
<tr><td> color           </td><td>
   [option] Color string for this source. 
   Color names those are shown by `M-x list-colors-display` or RGB hex format like "#abcdef".
</td></tr>
<tr><td> period-fgcolor  </td><td>
   [option] Foreground color for period items. The default color is white or black.
</td></tr>
<tr><td> period-bgcolor  </td><td>
   [option] Background color for period items. The default color is `cfw:source-color`.
</td></tr>
<tr><td> opt-face        </td><td>
   [option] Additional options for the normal item face.
   Ex. `:opt-face '(:weight bold)`
</td></tr>
<tr><td> opt-period-face </td><td> 
  [option] Additional options for the period item face.
</td></tr>
</table>

Only `name` and `data` slots are essential. Many slots are visual options.

In many cases, one has to specify only the `color` slot for visual, because the calfw chooses appropriate colors for the rest color options.

#### cfw:source-data details

This section explains what the function-slot `cfw:source-data` should return.

The function-slot `cfw:source-data` receives two arguments, start and end date of the query period, and returns an alist that consists of ([date] . ([item1] [item2] ... )).

Here is a simple example. 

cfw:source-data example1:

    ;; cfw:source-data example
    (defun sample-data1 (b e)
      '(
        ((1  1 2011) . ("item1"))
        ((1 10 2011) . ("item2-1" "item2-2"))
        ))
    
    (cfw:open-calendar-buffer
      :date (cfw:date 1 1 2011)
      :contents-sources
       (list 
         (make-cfw:source
          :name "test1" :data 'sample-data1)))

Evaluating this code in the scratch buffer, following result is displayed.

<a href="https://cacoo.com/diagrams/P6baUrxEQj4NYheV-50310.png">
<img src="https://cacoo.com/diagrams/P6baUrxEQj4NYheV-50310.png?width=450" />
</a>

The date is specified by `([month] [day] [year])`. This format is commonly used in calendar.el and orgmode.
(I diagrammed the exchange ways for some time and date formats in Emacs, [here](https://cacoo.com/diagrams/lsA64PTazlLTbSwR).)


Period items are little different. One period item is specified by `([start date] [end date] [content])` and the `periods` record of the alist collects them as a list, like the following code.

cfw:source-data example2:

    ;; cfw:source-data period items
    (defun sample-data2 (b e)
      '(
        ((1  8 2011) . ("item1"))
         (periods
          ((1 8 2011) (1 9 2011) "period item")
          ((1 11 2011) (1 12 2011) "next item"))
        ))
    ;; (A . (B C) ) is equivalent to (A B C)
    
    (cfw:open-calendar-buffer
      :date (cfw:date 1 1 2011)
      :contents-sources
       (list 
         (make-cfw:source
          :name "test2" :data 'sample-data2)))

Evaluating this code in the scratch buffer, following result is displayed.

<a href="https://cacoo.com/diagrams/P6baUrxEQj4NYheV-40315.png">
<img src="https://cacoo.com/diagrams/P6baUrxEQj4NYheV-40315.png?width=450" />
</a>

Here are other detailed specifications.

- The both start and end date are included by the query period.
- The items those aren't included in the query period are ignored.
- `cfw:source-data` should return a value as fast as possible, because users are waiting for the result. Caching is good idea.
- Schedule items don't have to be ordered. Duplicated items may be gathered.
- In the day cell, the items are sorted by `string-lessp`, i.e. numerical and alphabetical order.
  - The ordering function can be customized by the named argument `:sorter` of the component construction.

In the above examples, the dates of the schedule items are fixed. The actual sources generate result values by the programs. The codes of calfw add-ons may be helpful for your implementation.


### How to embed the calfw component in the other applications?

Calfw is built on the MVC architecture, using simple structure objects and modules employed by naming rules.

TODO...

#### Calfw component

##### Buffer

##### Region

##### Text


#### Calfw objects

##### Overview

##### cfw:component

##### cfw:model

##### cfw:dest


#### Application desgin


## History

- 2011/07/20 ver 1.2 : Merged many patches and improved many and bug fixed.
- 2011/07/05 ver 1.0 : Refactored the whole implementation and design. Improved UI and views.
- 2011/01/07 ver 0.2.1 : Supporting org-agenda schedules.
- 2011/01/07 ver 0.1 : First release. Supporting howm and iCal schedules.

--------------------------------------------------
SAKURAI, Masashi
m.sakurai atmark kiwanami.net

Time-stamp: <2011-08-24 01:04:28 sakurai>
