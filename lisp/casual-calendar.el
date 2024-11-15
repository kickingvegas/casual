;;; casual-calendar.el --- Transient UI for Calendar -*- lexical-binding: t; -*-

;; Copyright (C) 2024  Charles Choi

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

;; Casual Calendar is an opinionated Transient-based user interface for the
;; Emacs regular expression editor.

;; INSTALLATION
;; (require 'casual-calendar) ; optional if using autoloaded menu
;; (keymap-set calendar-mode-map "C-o" #'casual-calendar)

;; If you are using Emacs ≤ 30.0, you will need to update the built-in package
;; `transient'. By default, `package.el' will not upgrade a built-in package.
;; Set the customizable variable `package-install-upgrade-built-in' to `t' to
;; override this. For more details, please refer to the "Install" section on
;; this project's repository web page.

;;; Code:
(require 'casual-calendar-utils)
(require 'casual-calendar-settings)

;;;###autoload (autoload 'casual-calendar "casual-calendar" nil t)
(defun casual-calendar ()
  "Call Casual Calendar main menu.

Helper function for calling `casual-calendar-tmenu'. Calls
`calendar-redraw' to fix window sizing."
  (interactive)
  (call-interactively #'casual-calendar-tmenu)
  (calendar-redraw))

;;;###autoload (autoload 'casual-calendar-tmenu "casual-calendar" nil t)
(transient-define-prefix casual-calendar-tmenu ()
  "Transient menu for Calendar commands.

Main menu for `calendar' commands.

* References
- Info node `(emacs) Calendar/Diary'"
  casual-calendar--navigation-group

  [["Conversions"
    ("c" "Conversions›" casual-calendar-conversions-tmenu)
    ("A" "Convert to all" calendar-print-other-dates :transient t)
    ("i" "ISO Date" calendar-iso-print-date :transient t)]

   ["Holidays"
    ("H" "Holidays in span" calendar-list-holidays :transient t)
    ("h" "Holidays at point" calendar-cursor-holidays :transient t)
    ("x" "Mark Holidays" calendar-mark-holidays :transient t)
    ("u" "Unmark" calendar-unmark :transient t)]

   ["Misc"
    ("O" "Org Agenda" org-calendar-goto-agenda)
    ("d" "Diary" diary-view-entries :transient t)
    ("s" "All Diary" diary-show-all-entries :transient t)
    ("D" "Diary & Goto›" casual-calendar-diary-and-goto-tmenu)]

   ["Almanac"
    :pad-keys t
    ("M" "Lunar Phases" calendar-lunar-phases
     :description (lambda () (casual-calendar-unicode-get :lunar))
     :transient t)
    ("S" "Sunrise/Sunset" calendar-sunrise-sunset
     :description (lambda () (casual-calendar-unicode-get :sunrise))
     :transient t)
    ("M-m" "Sunrise/Sunset Month" calendar-sunrise-sunset-month
     :description (lambda () (format "%s Month" (casual-calendar-unicode-get :sunrise)))
     :transient t)]]

  [:class transient-row
   (casual-lib-quit-one)
   ("RET" "Dismiss" transient-quit-all)
   ("I" "ⓘ Info" (lambda ()
                   (interactive)
                   (calendar-exit)
                   (calendar-goto-info-node)))
   ("," "Settings›" casual-calendar-settings-tmenu)
   ("q" "Quit" calendar-exit)])

(provide 'casual-calendar)
;;; casual-calendar.el ends here
