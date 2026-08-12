;;; casual-agenda.el --- Transient UI for Agenda -*- lexical-binding: t; -*-

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

;; Casual Agenda is a Transient user interface for the Org Agenda library.

;; INSTALLATION

;; By default, `casual-init' will setup Casual Agenda by running the hook
;; function `casual-agenda-init'.

;; Ensure that `casual-agenda-init' is included in the customizable hook
;; variable `casual-init-hook'.

;; Consult the Info node `(casual) Agenda Install' for more detail on
;; installation.

;;; Code:
(require 'org-agenda)
(require 'bookmark)
(require 'casual-agenda-utils)
(require 'casual-agenda-settings)

;;;###autoload (autoload 'casual-agenda-init "casual-agenda" nil t)
(defun casual-agenda-init ()
  "Initialize and configure Casual Agenda.

This hook binds `casual-agenda-tmenu' to `casual-keybinding-primary'.

If `casual-agenda-add-extra-keybindings' is non-nil, then extra
keybindings specified in `casual-agenda-setup' will be set."
  (add-hook 'org-agenda-mode-hook #'casual-agenda-setup))

(defun casual-agenda-setup ()
  "Setup `org-agenda-mode' for Casual.

To see what keybindings are set by this function, press ‘s’ to view its
source."
  (keymap-set org-agenda-mode-map casual-keybinding-primary #'casual-agenda-tmenu)

  (when casual-agenda-add-extra-keybindings
    (keymap-set org-agenda-mode-map "M-j" #'org-agenda-clock-goto)
    (keymap-set org-agenda-mode-map "J" #'bookmark-jump)
    (keymap-set org-agenda-mode-map "." #'casual-agenda-goto-now)))

;;;###autoload (autoload 'casual-agenda-tmenu "casual-agenda" nil t)
(transient-define-prefix casual-agenda-tmenu ()
  "Transient menu for Org Agenda."
  :refresh-suffixes t
  ["Agenda"
   :class transient-row
   ("d" "Day" org-agenda-day-view
    :inapt-if-not casual-agenda-type-agendap
    :transient t)
   ("w" "Week" org-agenda-week-view
    :inapt-if-not casual-agenda-type-agendap
    :transient t)
   ("t" "Fortnight" org-agenda-fortnight-view
    :inapt-if-not casual-agenda-type-agendap
    :transient t)
   ("m" "Month" org-agenda-month-view
    :inapt-if-not casual-agenda-type-agendap
    :transient t)
   ("y" "Year" org-agenda-year-view
    :inapt-if-not casual-agenda-type-agendap
    :transient t)
   ("." "Now" casual-agenda-goto-now :transient t)]

  ["Filter"
   [("/" "Filter…" org-agenda-filter :transient t)
    ("=" "Regexp…" org-agenda-filter-by-regexp :transient t)]

   [("\\" "Tag…" org-agenda-filter-by-tag :transient t)
    ("^" "Headline…" org-agenda-filter-by-top-headline
     :inapt-if-not (lambda () (casual-agenda-headlinep))
     :transient t)]

   [("<" "Category…" org-agenda-filter-by-category
     :inapt-if-not (lambda () (casual-agenda-headlinep))
     :transient t)
    ("_" "Effort…" org-agenda-filter-by-effort :transient t)]

   [("|" "Remove all" org-agenda-filter-remove-all :transient t)]]


  ["Actions"
   :class transient-row
   ("o" "Operations›" casual-agenda-operations-tmenu)
   ("M" "Mark›" casual-agenda-mark-tmenu
    :inapt-if-not (lambda () (casual-agenda-headlinep)))
   ("s" "Save all" org-save-all-org-buffers :transient t)
   ("k" "Capture…" org-capture)
   ("a" "Agenda…" org-agenda)]

  casual-agenda-agenda-navigation-group

  ["Utils"
   :class transient-row
   (";" "⏱️" org-timer-set-timer
    :description (lambda () (format "%s…" (casual-agenda-unicode-get :timer)))
    :transient t)
   ("c" "📅" org-agenda-goto-calendar
    :inapt-if-not casual-agenda-type-agendap
    :description (lambda () (format "%s" (casual-agenda-unicode-get :date))))
   ("l" "Almanac›" casual-agenda-almanac-tmenu
    :inapt-if-not casual-agenda-type-datep)
   ("J" "Bookmark jump…" bookmark-jump
    :description (lambda () (format "%s…"
                                    (casual-agenda-unicode-get :jumpbookmark))))]
  [:class transient-row
   (casual-lib-quit-one)
   ("RET" "Open" org-agenda-switch-to)
   ("C-/" "Undo" org-agenda-undo)
   ("I" "ⓘ Info" org-info-find-node)
   ("," "Settings›" casual-agenda-settings-tmenu)
   ("q" "Quit" org-agenda-quit)])

(transient-define-prefix casual-agenda-almanac-tmenu ()
  "Almanac menu."
  :refresh-suffixes t
  ["Almanac"
   :class transient-row
   ("S" "🌅" org-agenda-sunrise-sunset
    :inapt-if-not casual-agenda-type-datep
    :description (lambda () (format "%s" (casual-agenda-unicode-get :sunrise)))
    :transient t)
   ("M" "🌙" org-agenda-phases-of-moon
    :inapt-if-not casual-agenda-type-datep
    :description (lambda () (format "%s" (casual-agenda-unicode-get :lunar))))
   ("H" "Holidays" org-agenda-holidays
    :inapt-if-not casual-agenda-type-datep)]

  casual-agenda-agenda-navigation-group
  casual-agenda-navigation-group)


(transient-define-prefix casual-agenda-operations-tmenu ()
  :refresh-suffixes t
  ["Operations"
   :pad-keys t
   :inapt-if-not (lambda () (casual-agenda-headlinep))
   [("t" "Todo…" org-agenda-todo :transient t)
    (":" "Tags…" org-agenda-set-tags :transient t)
    ("B" "Bulk Action…" org-agenda-bulk-action :transient t)]
   [("s" "Schedule…" org-agenda-schedule :transient t)
    ("d" "Deadline…" org-agenda-deadline :transient t)]
   [("+" "↑ Priority" org-agenda-priority-up
     :description (lambda () (format "%s Priority" (casual-agenda-unicode-get :up)))
     :transient t)
    ("-" "↓ Priority" org-agenda-priority-down
     :description (lambda () (format "%s Priority" (casual-agenda-unicode-get :down)))
     :transient t)]
   [("R" "Refile…" org-agenda-refile)
    ("z" "Add Note" org-agenda-add-note)]
   [("S" "Set Property…" org-agenda-set-property)
    ("A" "Archive…" org-agenda-archive-default-with-confirmation)]]

  ["Clock"
   :class transient-row
   ("I" "Clock In" casual-agenda-clock-in
    :inapt-if-not (lambda () (casual-agenda-headlinep))
    :transient t)
   ("O" "Clock Out" casual-agenda-clock-out
    :inapt-if-not org-clocking-p
    :transient t)
   ("x" "Cancel" casual-agenda-clock-cancel
    :inapt-if-not org-clocking-p
    :transient t)
   ("m" "Modify" org-clock-modify-effort-estimate
    :inapt-if-not org-clocking-p
    :transient t)]

  casual-agenda-agenda-navigation-group
  casual-agenda-navigation-group)

(transient-define-prefix casual-agenda-mark-tmenu ()
  ["Mark"
   :pad-keys t
   [("m" "Mark" org-agenda-bulk-mark :transient t)
    ("x" "Mark Regexp…" org-agenda-bulk-mark-regexp :transient t)]
   [("u" "Unmark" org-agenda-bulk-unmark :transient t)
    ("U" "Unmark" org-agenda-bulk-unmark-all :transient t)]
   [("t" "Toggle" org-agenda-bulk-toggle :transient t)
    ("T" "Toggle all" org-agenda-bulk-toggle-all :transient t)]
   [("B" "Bulk Action…" org-agenda-bulk-action :transient t)]]

  casual-agenda-agenda-navigation-group
  casual-agenda-navigation-group)

(provide 'casual-agenda)
;;; casual-agenda.el ends here
