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
  <tr><td>  TAB                  </td><td> Next item in a day</td></tr>
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

![howm menu embedding](https://cacoo.com/diagrams/vrScI4K2QlmDApfd-1F941.png?width=450)

### For org users:

    (require 'calfw-org)

Then, M-x cfw:open-org-calendar.

![org-agenda and calfw-org](https://cacoo.com/diagrams/S6aJntG6giGs44Yn-89CB2.png?width=450)

### For iCal (Google Calendar) users:

Here is a minimum sample code:


    (require 'calfw-ical)
    (cfw:open-ical-calendar "http://www.google.com/calendar/ical/.../basic.ics")

![Google Calendar and calfw-ical](https://cacoo.com/diagrams/vrScI4K2QlmDApfd-5E808.png?width=450)

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

One can customize the faces.

Here is a template code for face customization:

    (custom-set-faces
     '(cfw:face-title ((t (:foreground "#f0dfaf" :weight bold :height 2.0 :inherit variable-pitch))))
     '(cfw:face-header ((t (:foreground "#d0bf8f" :weight bold))))
     '(cfw:face-sunday ((t :foreground "#cc9393" :background "grey10" :weight bold)))
     '(cfw:face-saturday ((t :foreground "#8cd0d3" :background "grey10" :weight bold)))
     '(cfw:face-holiday ((t :background "grey10" :foreground "#8c5353" :weight bold)))
     '(cfw:face-grid ((t :foreground "DarkGrey")))
     '(cfw:face-default-content ((t :foreground "#bfebbf")))
     '(cfw:face-periods ((t :foreground "cyan")))
     '(cfw:face-day-title ((t :background "grey10")))
     '(cfw:face-default-day ((t :weight bold :inherit cfw:face-day-title)))
     '(cfw:face-annotation ((t :foreground "RosyBrown" :inherit cfw:face-day-title)))
     '(cfw:face-disable ((t :foreground "DarkGray" :inherit cfw:face-day-title)))
     '(cfw:face-today-title ((t :background "#7f9f7f" :weight bold)))
     '(cfw:face-today ((t :background: "grey10" :weight bold)))
     '(cfw:face-select ((t :background "#2f2f2f")))
     '(cfw:face-toolbar ((t :foreground "Steelblue4" :background "Steelblue4")))
     '(cfw:face-toolbar-button-off ((t :foreground "Gray10" :weight bold)))
     '(cfw:face-toolbar-button-on ((t :foreground "Gray50" :weight bold))))

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

### Line breaking

If a content string is longer than the cell width, the calfw breaks into the multiple lines.
In the current implementation, the Calfw has 3 strategies: none, simple and wordwrap. The variable `cfw:render-line-breaker` selects the strategy to break lines.

- `cfw:render-line-breaker-none`
 - Never breaks lines. Longer contents are truncated.
- `cfw:render-line-breaker-simple` (default)
 - This strategy breaks lines with rigid width. This may be not so beautiful, but In the most cases it looks good.
- `cfw:render-line-breaker-wordwrap`
 - This strategy breaks lines with the emacs function `fill-region`. Although, the line breaking algorithm of the Emacs is not so smart as more complicated ones, such as Knuth/Plass algorithm, this strategy is better than the simple one.

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

![Simple source example](https://cacoo.com/diagrams/P6baUrxEQj4NYheV-50310.png?width=450)

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

![Range items example](https://cacoo.com/diagrams/P6baUrxEQj4NYheV-40315.png?width=450)

Here are other detailed specifications.

- The both start and end date are included by the query period.
- The items those aren't included in the query period are ignored.
- `cfw:source-data` should return a value as fast as possible, because users are waiting for the result. Caching is good idea.
- Schedule items don't have to be ordered. Duplicated items may be gathered.
- In the day cell, the items are sorted by `string-lessp`, i.e. numerical and alphabetical order.
  - The ordering function can be customized by the named argument `:sorter` of the component construction.

In the above examples, the dates of the schedule items are fixed. The actual sources generate result values by the programs. The codes of calfw add-ons may be helpful for your implementation.


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

The function `cfw:open-calendar-buffer` makes a new calendar buffer (calfw buffer) and displays it by `switch-to-buffer`. The major mode of the calfw buffer is `cfw:calendar-mode` and the keymap `cfw:calendar-mode-map` is bound.

This destination is easy to use for applications and users, because the buffer is usual application boundary and users know how to use buffers.

##### Region

The 'Region' destination embeds the calendar view in the buffer which is managed by the other applications. This destination can give the other applications a nice calendar view. See the howm embedding for example.

Let's try a demonstration. Evaluate this code in your scratch buffer.

Region destination example:

    ;; Evaluate this code in the scratch buffer
    (require 'calfw)
    (cfw:create-calendar-component-region :height 10)

Then, the calendar view will be embedded in the scratch buffer like the following screenshot. You can navigate the calfw view in the buffer. Undoing for the some times, you can remove the calfw view.

![calfw in the scratch buffer](https://cacoo.com/diagrams/P6baUrxEQj4NYheV-B9649.png?width=600)

Because this destination never interacts anything out of the region and has its own key-binds as a text property, users can easily embed a calendar view in the other applications.

##### Text

The 'text' destination generates just a text which represent calfw view. The function `cfw:get-calendar-text` returns the text.

##### Destination and View

Three destinations are explained as mentioned above. Although they have different appearance, the application can operate the calfw component in the same way.

Let us call them 'destination', it is the abstraction of UI components.

The similar word 'view' means in which form the calfw displays the contents, for example, monthly form, two-weeks and weekly one and etc.

#### Calfw objects

##### Overview

The calfw consists of four objects:

- `cfw:component` that gathers following objects up.
- `cfw:model` that manages calendar contents.
- `cfw:source` that defines schedule items.
- `cfw:dest` that is abstraction of destinations.

The relations between the objects are displayed as UML class diagram ([Diagrammed by astah](http://astah.change-vision.com/ja/:title=Astah)).

![Overview for calfw objects](https://cacoo.com/diagrams/P6baUrxEQj4NYheV-EC8C6.png)

`cfw:component` acts as Controller of MVC. It connects model object and destination one, and controls all events. It also gives the interface of calfw objects for the other applications.

`cfw:model` and `cfw:source` act as Model of MVC. They manage the schedule contents and calendar logics.

`cfw:dest` acts as View of MVC. It abstracts the common interface from UI destinations.

##### cfw:component

The object `cfw:component` controls calfw objects and events.

The object has following information:

- References to `cfw:dest` object and `cfw:model` one.
- Selected date on the calfw component.
- View style.
- Hooks
  - `update-hooks`
  - `selection-change-hooks`
  - `click-hooks`.

The object has following operations:

- Getting object references to `cfw:dest`, `cfw:model`, belonging buffer and so on.
- Getting and setting the selected date (`get-selected-date` / `set-selected-date`).
- Getting and setting the view style (`get-view` / `set-view`).
  - The view style is a symbol, such as `month`, `two-weeks`, `week` and `day`.
- Resizing and refreshing the view (`resize` / `update`).
- Managing hooks (`add-xxx-hook` / `remove-xxx-hook`)

After construction of the calfw component, the destination object can not be changed.

The views are defined as a function and dispatched by the function `cfw:cp-dispatch-view-impl`.

The instance of the calfw component is stored at following places:

- `buffer` destination: the buffer-local variable `cfw:component`
- `region` destination: the text property `cfw:component`
- `text` destination: N/A

Calling the utility function `cfw:cp-get-component`, one can obtain the calfw instance at the appropriate places. The stateless functions, such as simple event handler functions, can use this function to get the instance.

The applications those have the state-full operations, however, should hold their own calfw instance for the safety object reference.

##### cfw:model

The object `cfw:model` gathers schedule sources and gives a common interface for view functions to access the contents.

The object has following information:

- contents source objects (`contents-sources`)
- annotation source objects (`annotation-sources`)
- sorting function (`sorter`)

The model object has no information of views and destinations, just manages schedule contents.

The holidays are retrieved from the global function `calendar-holiday-list` of calendar.el.

The schedule contents are modified through the model object after the component construction.

(In the current implementation, the model object is build by alist. Then, view functions adds some data as view model. I think it is not good solution, so the implementation may be modified in future.)

##### cfw:dest

The object `cfw:dest` abstracts rendering destinations and gives a common interface of rendering operation to view functions.

The object has following information:

- destination buffer object (`buffer`)
- region functions (`min-func`, `max-func`)
- reference size (`width`, `height`)
- clearing function (`clear-func`)
- advice functions (`before-update-func`, `after-update-func`)
- overlay data (`select-ol`, `today-ol`)

In the current implementation, `cfw:dest` has three forms, buffer, region and text, mentioned above. Actually, the region destination is what I want. One buffer can have some destination objects, because all data (including local-variables and keymaps) are packed in the `cfw:dest` object.

#### Application desgin

In this section, I would describe a simple guide line of application design using calfw.

One can use calfw as an application UI (like calfw-howm) or dialog UI for selecting a date (like calendar.el). The user application can choose the destination style: buffer or region. Switching between them is very easy.

The data presentation can be achieved by defining `cfw:source` object. It may be straightforward.

The input events by the user can be caught by hooks in the `cfw:component`. Then, the selected date is obtained by the function `cfw:cursor-to-nearest-date` or `cfw:cursor-to-date`. The current implementation, calfw can not treat a range on the calendar.

Generally, any events can be caught by the custom keymap which is given by the named argument `:custom-map` with component construction. Furthermore, because calfw reserves the text properties (face, keymap and so on) on the text that is returned by `cfw:source` objects, one can control event handling at each characters.

Once the model is modified, update function of the `cfw:component` object should be called to refresh the view.

The summary diagram is here.

![Summary of application design](https://cacoo.com/diagrams/P6baUrxEQj4NYheV-465D4.png)

See the calfw-howm.el code for more details.

## History

- 2011/10/10 ver 1.3 : Improved visual and navigations: multi-line, moving items in a day, diary mode and so on.
- 2011/07/20 ver 1.2 : Merged many patches and improved many and bug fixed.
- 2011/07/05 ver 1.0 : Refactored the whole implementation and design. Improved UI and views.
- 2011/01/07 ver 0.2.1 : Supporting org-agenda schedules.
- 2011/01/07 ver 0.1 : First release. Supporting howm and iCal schedules.

--------------------------------------------------
SAKURAI, Masashi
m.sakurai atmark kiwanami.net

Time-stamp: <2011-10-10 17:08:30 sakurai>
