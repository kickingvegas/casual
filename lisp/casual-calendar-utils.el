;;; casual-calendar-utils.el --- Casual Calendar Utils -*- lexical-binding: t; -*-

;; Copyright (C) 2024-2025 Charles Y. Choi

;; Author: Charles Choi <kickingvegas@gmail.com>
;; Keywords: tools

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:
;;

;;; Code:
(require 'casual-calendar-constants)

(transient-define-prefix casual-calendar-diary-and-goto-tmenu ()
  "Diary Entry Insertion & Goto Date menu.

Diary entry insertion and general goto date commands are offered
by this menu."

  ["Diary and Goto"
   ["Diary Insert"
    ("e" "Entry" diary-insert-entry)
    ("w" "Weekly" diary-insert-weekly-entry)
    ("m" "Monthly" diary-insert-monthly-entry)
    ("y" "Yearly" diary-insert-yearly-entry)
    ("a" "Anniversary" diary-insert-anniversary-entry)]

   ["Goto"
    ("g" "Date…" calendar-goto-date :transient t)
    ("i" "ISO Date…" calendar-iso-goto-date :transient t)
    ("d" "Day of Year…" calendar-goto-day-of-year :transient t)]]

  casual-calendar--menu-navigation-group)

(transient-define-prefix casual-calendar-conversions-tmenu ()
  "Casual Calendar Conversions Menu.

Selection of different calendar systems.

- Astronomical
- Bahá’í
- Ethiopic
- French Revolutionary
- Hebrew
- Islamic
- Julian
- Lunar (Chinese)
- Mayan
- Persian

Consult info node `(emacs) Other Calendars' for more detail on
specific supported non-Gregorian calendar system behavior."

  ["Calendars"
   [("a" "Astronomical›" casual-calendar-astro-tmenu)
    ("b" "Bahá’í›" casual-calendar-bahai-tmenu)
    ("c" "Coptic›" casual-calendar-coptic-tmenu)]

   [("e" "Ethiopic›" casual-calendar-ethiopic-tmenu)
    ("f" "French Revolutionary›" casual-calendar-french-tmenu)
    ("h" "Hebrew›" casual-calendar-hebrew-tmenu)]

   [("i" "Islamic›" casual-calendar-islamic-tmenu)
    ("j" "Julian›" casual-calendar-julian-tmenu)
    ("l" "Lunar (Chinese)›" casual-calendar-lunar-tmenu)]

   [("m" "Mayan›" casual-calendar-mayan-tmenu)
    ("p" "Persian›" casual-calendar-persian-tmenu)]]

  ["All"
   ("A" "Convert to all" calendar-print-other-dates :transient t)]

  [:class transient-row
   (casual-lib-quit-one)
   ("RET" "Dismiss" transient-quit-all)
   ("I" "ⓘ Info" (lambda ()
                   (interactive)
                   (calendar-exit)
                   (info "(emacs) Other Calendars")))
   (casual-lib-quit-all)])

(transient-define-prefix casual-calendar-lunar-tmenu ()
  "Casual Calendar Lunar (Chinese) Calendar Menu.

This menu provides date conversion commands between a Gregorian
and a Lunar (Chinese) calendar.

To convert a Gregorian date to Lunar (Chinese):

  1. Move cursor (point) in Calendar window to desired date.
     (The command “(g) Goto…” can be used to accomplish this.)

  2. Choose “(c) Date at Cursor”.

To convert a Lunar (Chinese) date to Gregorian:

  1. Choose “(G) Goto…” and follow the prompts.

Diary insertion of Lunar (Chinese) events are supported provided
proper configuration of the variables
`diary-nongregorian-listing-hook' and
`diary-nongregorian-marking-hook' as detailed in info
node `(emacs) Non-Gregorian Diary'.

Both of these variables can be configured from the Casual Calendar
menu `casual-calendar-settings-tmenu'."

  ["Lunar (Chinese) Calendar"
   ["Date"
    ("c" "Date at cursor" calendar-chinese-print-date :transient t)
    ("G" "Goto…" calendar-chinese-goto-date :transient t)]

   ["Diary Insert"
    ("i" "Point" diary-chinese-insert-entry)
    ("m" "Monthly" diary-chinese-insert-monthly-entry)
    ("y" "Year" diary-chinese-insert-yearly-entry)
    ("A" "Anniversary" diary-chinese-insert-anniversary-entry)]

   ["Diary View"
    ("d" "View" diary-view-entries :transient t)
    ("s" "Show all" diary-show-all-entries)]]

  casual-calendar--navigation-group
  casual-calendar--menu-navigation-group)

(transient-define-prefix casual-calendar-astro-tmenu ()
  "Casual Calendar Astronomical calendar menu.

This menu provides date conversion commands between a Gregorian
and a Astronomical calendar.

To convert a Gregorian date to Astronomical:

  1. Move cursor (point) in Calendar window to desired date.
     (The command “(g) Goto…” can be used to accomplish this.)

  2. Choose “(c) Date at Cursor”.

To convert a Astronomical date to Gregorian:

  1. Choose “(G) Goto…” and follow the prompts."

  ["Astronomical Calendar"
   ["Date"
    ("c" "Day number at cursor" calendar-astro-print-day-number :transient t)
    ("G" "Goto…" calendar-astro-goto-day-number :transient t)]

   ["Diary View"
    ("d" "View" diary-view-entries :transient t)
    ("s" "Show all" diary-show-all-entries)]]

  casual-calendar--navigation-group
  casual-calendar--menu-navigation-group)

(transient-define-prefix casual-calendar-islamic-tmenu ()
  "Casual Calendar Islamic calendar menu.

This menu provides date conversion commands between a Gregorian
and an Islamic calendar.

To convert a Gregorian date to Islamic:

  1. Move cursor (point) in Calendar window to desired date.
     (The command “(g) Goto…” can be used to accomplish this.)

  2. Choose “(c) Date at Cursor”.

To convert an Islamic date to Gregorian:

  1. Choose “(G) Goto…” and follow the prompts.

Diary insertion of Islamic events are supported provided
proper configuration of the variables
`diary-nongregorian-listing-hook' and
`diary-nongregorian-marking-hook' as detailed in info
node `(emacs) Non-Gregorian Diary'.

Both of these variables can be configured from the Casual Calendar
menu `casual-calendar-settings-tmenu'."
  ["Islamic Calendar"
   ["Date"
    ("c" "Date at cursor" calendar-islamic-print-date :transient t)
    ("G" "Goto…" calendar-islamic-goto-date :transient t)]

   ["Diary Insert"
    ("i" "Point" diary-islamic-insert-entry)
    ("m" "Monthly" diary-islamic-insert-monthly-entry)
    ("y" "Year" diary-islamic-insert-yearly-entry)]

   ["Diary View"
    ("d" "View" diary-view-entries :transient t)
    ("s" "Show all" diary-show-all-entries)]]

  casual-calendar--navigation-group
  casual-calendar--menu-navigation-group)

(transient-define-prefix casual-calendar-hebrew-tmenu ()
  "Casual Calendar Hebrew calendar menu.

This menu provides date conversion commands between a Gregorian
and a Hebrew calendar.

To convert a Gregorian date to Hebrew:

  1. Move cursor (point) in Calendar window to desired date.
     (The command “(g) Goto…” can be used to accomplish this.)

  2. Choose “(c) Date at Cursor”.

To convert a Hebrew date to Gregorian:

  1. Choose “(G) Goto…” and follow the prompts.

Diary insertion of Hebrew events are supported provided
proper configuration of the variables
`diary-nongregorian-listing-hook' and
`diary-nongregorian-marking-hook' as detailed in info
node `(emacs) Non-Gregorian Diary'.

Both of these variables can be configured from the Casual Calendar
menu `casual-calendar-settings-tmenu'."

  ["Hebrew Calendar"
   ["Date"
    ("c" "Date at cursor" calendar-hebrew-print-date :transient t)
    ("G" "Goto…" calendar-hebrew-goto-date :transient t)]

   ["Diary Insert"
    ("i" "Point" diary-hebrew-insert-entry)
    ("m" "Monthly" diary-hebrew-insert-monthly-entry)
    ("y" "Year" diary-hebrew-insert-yearly-entry)]

   ["Diary View"
    ("d" "View" diary-view-entries :transient t)
    ("s" "Show all" diary-show-all-entries)]]

  casual-calendar--navigation-group
  casual-calendar--menu-navigation-group)

(transient-define-prefix casual-calendar-bahai-tmenu ()
  "Casual Calendar Bahá’í calendar menu.

This menu provides date conversion commands between a Gregorian
and a Bahá’í calendar.

To convert a Gregorian date to Bahá’í:

  1. Move cursor (point) in Calendar window to desired date.
     (The command “(g) Goto…” can be used to accomplish this.)

  2. Choose “(c) Date at Cursor”.

To convert a Bahá’í date to Gregorian:

  1. Choose “(G) Goto…” and follow the prompts.

Diary insertion of Bahá’í events are supported provided
proper configuration of the variables
`diary-nongregorian-listing-hook' and
`diary-nongregorian-marking-hook' as detailed in info
node `(emacs) Non-Gregorian Diary'.

Both of these variables can be configured from the Casual Calendar
menu `casual-calendar-settings-tmenu'."

  ["Bahá’í Calendar"
   ["Date"
    ("c" "Date at cursor" calendar-bahai-print-date :transient t)
    ("G" "Goto…" calendar-bahai-goto-date :transient t)]

   ["Diary Insert"
    ("i" "Point" diary-bahai-insert-entry)
    ("m" "Monthly" diary-bahai-insert-monthly-entry)
    ("y" "Year" diary-bahai-insert-yearly-entry)]

   ["Diary View"
    ("d" "View" diary-view-entries :transient t)
    ("s" "Show all" diary-show-all-entries)]]

  casual-calendar--navigation-group
  casual-calendar--menu-navigation-group)

(transient-define-prefix casual-calendar-ethiopic-tmenu ()
  "Casual Calendar Ethiopic calendar menu.

This menu provides date conversion commands between a Gregorian
and an Ethiopic calendar.

To convert a Gregorian date to Ethiopic:

  1. Move cursor (point) in Calendar window to desired date.
     (The command “(g) Goto…” can be used to accomplish this.)

  2. Choose “(c) Date at Cursor”.

To convert an Ethiopic date to Gregorian:

  1. Choose “(G) Goto…” and follow the prompts."

  ["Ethiopic Calendar"
   ["Date"
    ("c" "Date at cursor" calendar-ethiopic-print-date :transient t)
    ("G" "Goto…" calendar-ethiopic-goto-date :transient t)]

   ["Diary View"
    ("d" "View" diary-view-entries :transient t)
    ("s" "Show all" diary-show-all-entries)]]

  casual-calendar--navigation-group
  casual-calendar--menu-navigation-group)

(transient-define-prefix casual-calendar-french-tmenu ()
  "Casual Calendar French Revolutionary calendar menu.

This menu provides date conversion commands between a Gregorian
and a French Revolutionary calendar.

To convert a Gregorian date to French Revolutionary:

  1. Move cursor (point) in Calendar window to desired date.
     (The command “(g) Goto…” can be used to accomplish this.)

  2. Choose “(c) Date at Cursor”.

To convert a French Revolutionary date to Gregorian:

  1. Choose “(G) Goto…” and follow the prompts."

  ["French Revolutionary Calendar"
   ["Date"
    ("c" "Date at cursor" calendar-french-print-date :transient t)
    ("G" "Goto…" calendar-french-goto-date :transient t)]

   ["Diary View"
    ("d" "View" diary-view-entries :transient t)
    ("s" "Show all" diary-show-all-entries)]]

  casual-calendar--navigation-group
  casual-calendar--menu-navigation-group)

(transient-define-prefix casual-calendar-julian-tmenu ()
  "Casual Calendar Julian calendar menu.

This menu provides date conversion commands between a Gregorian
and a Julian calendar.

To convert a Gregorian date to Julian:

  1. Move cursor (point) in Calendar window to desired date.
     (The command “(g) Goto…” can be used to accomplish this.)

  2. Choose “(c) Date at Cursor”.

To convert a Julian date to Gregorian:

  1. Choose “(G) Goto…” and follow the prompts."

  ["Julian Calendar"
   ["Date"
    ("c" "Date at cursor" calendar-julian-print-date :transient t)
    ("G" "Goto…" calendar-julian-goto-date :transient t)]

   ["Diary View"
    ("d" "View" diary-view-entries :transient t)
    ("s" "Show all" diary-show-all-entries)]]

  casual-calendar--navigation-group
  casual-calendar--menu-navigation-group)

(transient-define-prefix casual-calendar-coptic-tmenu ()
  "Casual Calendar Coptic calendar menu.

This menu provides date conversion commands between a Gregorian
and a Coptic calendar.

To convert a Gregorian date to Coptic:

  1. Move cursor (point) in Calendar window to desired date.
     (The command “(g) Goto…” can be used to accomplish this.)

  2. Choose “(c) Date at Cursor”.

To convert a Coptic date to Gregorian:

  1. Choose “(G) Goto…” and follow the prompts."

  ["Coptic Calendar"
   ["Date"
    ("c" "Date at cursor" calendar-coptic-print-date :transient t)
    ("G" "Goto…" calendar-coptic-goto-date :transient t)]

   ["Diary View"
    ("d" "View" diary-view-entries :transient t)
    ("s" "Show all" diary-show-all-entries)]]

  casual-calendar--navigation-group
  casual-calendar--menu-navigation-group)

(transient-define-prefix casual-calendar-persian-tmenu ()
  "Casual Calendar Persian calendar menu.

This menu provides date conversion commands between a Gregorian
and a Persian calendar.

To convert a Gregorian date to Persian:

  1. Move cursor (point) in Calendar window to desired date.
     (The command “(g) Goto…” can be used to accomplish this.)

  2. Choose “(c) Date at Cursor”.

To convert a Persian date to Gregorian:

  1. Choose “(G) Goto…” and follow the prompts."

  ["Persian Calendar"
   ["Date"
    ("c" "Date at cursor" calendar-persian-print-date :transient t)
    ("G" "Goto…" calendar-persian-goto-date :transient t)]

   ["Diary View"
    ("d" "View" diary-view-entries :transient t)
    ("s" "Show all" diary-show-all-entries)]]

  casual-calendar--navigation-group
  casual-calendar--menu-navigation-group)

(transient-define-prefix casual-calendar-mayan-tmenu ()
  "Casual Calendar Mayan calendar menu.

This menu provides date conversion commands between a Gregorian
and a Mayan calendar.

To convert a Gregorian date to Mayan:

  1. Move cursor (point) in Calendar window to desired date.
     (The command “(g) Goto…” can be used to accomplish this.)

  2. Choose “(c) Date at Cursor”.

To convert a Mayan date to Gregorian:

  1. Choose “(G) Goto…” and follow the prompts."

  ["Mayan Calendar"
   ["Date"
    ("c" "Date at cursor" calendar-mayan-print-date :transient t)
    ("G" "Goto long count…" calendar-mayan-goto-long-count-date :transient t)]

   ["Diary View"
    ("d" "View" diary-view-entries :transient t)
    ("s" "Show all" diary-show-all-entries)]]

  casual-calendar--navigation-group
  casual-calendar--menu-navigation-group)

(provide 'casual-calendar-utils)
;;; casual-calendar-utils.el ends here
