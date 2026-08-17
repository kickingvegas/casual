;;; casual-calendar.el --- Transient UI for Calendar -*- lexical-binding: t; -*-

;; Copyright (C) 2024-2026  Charles Y. Choi

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

;; Casual Calendar is a Transient user interface for the Calendar library.

;; INSTALLATION

;; By default, `casual-init' will setup Casual Calendar by running the hook
;; function `casual-calendar-init'.

;; Ensure that `casual-calendar-init' is included in the customizable hook
;; variable `casual-init-hook'.

;; Consult the Info node `(casual) Calendar Install' for more detail on
;; installation.

;;; Code:
(require 'casual-calendar-utils)
(require 'casual-calendar-settings)

;;;###autoload (autoload 'casual-calendar-init "casual-calendar" nil t)
(defun casual-calendar-init ()
  "Initialize and configure Casual Calendar.

This hook binds `casual-calendar-tmenu' to `casual-keybinding-primary'.

If `casual-calendar-add-extra-keybindings' is non-nil, then extra
keybindings specified in `casual-calendar-setup' will be set."
  (add-hook 'calendar-mode-hook #'casual-calendar-setup))

(defun casual-calendar-setup ()
  "Setup `calendar-mode' for Casual.

To see what keybindings are set by this function, press ‘s’ to view its
source."
  (keymap-set calendar-mode-map casual-keybinding-primary #'casual-calendar-tmenu))

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

  ["Region"
   :class transient-row
   ("C-SPC" "Set Mark" calendar-set-mark :transient t)
   ("=" "Count Days" calendar-count-days-region :transient t)]

  [:class transient-row
   (casual-lib-quit-one)
   ("," "Settings›" casual-calendar-settings-tmenu)
   ("I" "ⓘ" (lambda ()
              (interactive)
              (calendar-exit)
              (calendar-goto-info-node)))
   ("RET" "Done" transient-quit-all)
   ("q" "Quit" calendar-exit)])

(provide 'casual-calendar)
;;; casual-calendar.el ends here
