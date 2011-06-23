# Calfw - A calendar framework for Emacs

## What is calfw?

This program displays a calendar view in the Emacs buffer.

![Calfw image](https://cacoo.com/diagrams/vrScI4K2QlmDApfd-946E8.png)

## Installation

To use this program, locate this file to load-path directory,
and add the following code to your .emacs.

    (require 'calfw)

Executing the command `cfw:open-calendar-buffer', switch to the calendar buffer.
You can navigate the date like calendar.el.

Schedule data which are shown in the calendar view, are collected
by the variables `cfw:contents-functions` and
`cfw:annotations-functions`. The former variable defines schedule
contents. The later one does date annotations like the moon phases.

This program gets the holidays using the function
`calendar-holiday-list`. See the document for the holidays.el and the Info text.

## Add-ons:

Following programs are also useful:

- calfw-howm.el : Display howm schedules (http://howm.sourceforge.jp/index.html)
- calfw-ical.el : Display schedules of the iCalendar format, such as the google calendar.
- calfw-org.el  : Display org schedules (http://orgmode.org/)

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
    (cfw:install-org-schedules) 

Then, M-x cfw:open-org-calendar.

![org-agenda and calfw-org](https://cacoo.com/diagrams/S6aJntG6giGs44Yn-89CB2.png)

### For ical users:

Here is a minimum sample code:


    (require 'calfw-ical)
    (cfw:install-ical-schedules)
    (setq cfw:ical-calendar-contents-sources '("http://www.google.com/calendar/ical/.../basic.ics"))

![Google Calendar and calfw-ical](https://cacoo.com/diagrams/vrScI4K2QlmDApfd-5E808.png)

Executing the following command, this program clears caches to refresh the ICS data.

    (cfw:ical-calendar-clear-cache)

Or add the following advice:

    (defadvice cfw:refresh-calendar-buffer (before activate)
       (cfw:ical-calendar-clear-cache))

--------------------------------------------------

SAKURAI, Masashi
m.sakurai atmark kiwanami.net
