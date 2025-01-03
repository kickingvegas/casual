;;; casual-agenda-utils.el --- Casual Agenda Utils -*- lexical-binding: t; -*-

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
(require 'org-macs)
(require 'org-clock)
(require 'org-agenda)
(require 'casual-lib)

(defconst casual-agenda-unicode-db
  '((:previous . '("↑" "Prev"))
    (:next . '("↓" "Next"))
    (:up . '("↑" "Up"))
    (:down . '("↓" "Down"))
    (:jumpdate . '("🚀 📅" "Date"))
    (:jumpclocked . '("🚀 ⏰" "Clocked"))
    (:jump . '("🚀" "Jump"))
    (:marked . '("❯" "Marked"))
    (:date . '("📅" "Date"))
    (:heading . '("✲" "*"))
    (:timer . '("⏱️" "Timer"))
    (:sunrise . '("🌅" "Sunrise"))
    (:lunar . '("🌙" "Lunar"))
    (:jumpbookmark . '("🚀 🔖" "Jump to bookmark"))
    (:clock . '("⏰" "Clock")))
  "Unicode symbol DB to use for Agenda Transient menus.")

(defun casual-agenda-unicode-get (key)
  "Lookup Unicode symbol for KEY in DB.

- KEY symbol used to lookup Unicode symbol in DB.

If the value of customizable variable `casual-lib-use-unicode'
is non-nil, then the Unicode symbol is returned, otherwise a
plain ASCII-range string."
  (casual-lib-unicode-db-get key casual-agenda-unicode-db))

(defconst casual-agenda-navigation-group
  [:class transient-row
   (casual-lib-quit-one)
   ("RET" "Open" org-agenda-switch-to)
   ("." "Now" casual-agenda-goto-now :transient t)
   ("C-/" "Undo" org-agenda-undo :transient t)
   (casual-lib-quit-all)]
  "Casual Agenda menu navigation group.")

(defconst casual-agenda-agenda-navigation-group
  ["Navigation"
    ["Line"
     ("p" "↑" org-agenda-previous-line
      :description (lambda () (casual-agenda-unicode-get :previous))
      :transient t)
     ("n" "↓" org-agenda-next-line
      :description (lambda () (casual-agenda-unicode-get :next))
      :transient t)]

    ["Heading"
     ("P" "↑ ✲" org-agenda-previous-item
      :description (lambda () (format "%s %s"
                                      (casual-agenda-unicode-get :previous)
                                      (casual-agenda-unicode-get :heading)))
      :transient t)
     ("N" "↓ ✲" org-agenda-next-item
      :description (lambda () (format "%s %s"
                                      (casual-agenda-unicode-get :next)
                                      (casual-agenda-unicode-get :heading)))
      :transient t)]

    ["Date"
     :inapt-if-not casual-agenda-type-agendap
     ("M-p" "↑ 📅" org-agenda-previous-date-line
      :description (lambda () (format "%s %s"
                                      (casual-agenda-unicode-get :previous)
                                      (casual-agenda-unicode-get :date)))
      :transient t)
     ("M-n" "↓ 📅" org-agenda-next-date-line
      :description (lambda () (format "%s %s"
                                      (casual-agenda-unicode-get :next)
                                      (casual-agenda-unicode-get :date)))
      :transient t)]

    ["Calendar"
     :inapt-if-not casual-agenda-type-agendap
     ("b" "← earlier" org-agenda-earlier :transient t)
     ("f" "→ later" org-agenda-later :transient t)]

    ["Refresh"
     ("r" "Redo" org-agenda-redo :transient t)
     ("g" "Redo all" org-agenda-redo-all :transient t)]

    ["Jump"
     :pad-keys t
     ("j" "🚀 📅" org-agenda-goto-date
      :inapt-if-not casual-agenda-type-agendap
      :description (lambda () (format "%s…"
                                      (casual-agenda-unicode-get :jumpdate))))
     ("M-j" "🚀 ⏰" org-agenda-clock-goto
      :inapt-if-not org-clocking-p
      :description (lambda () (format "%s…" (casual-agenda-unicode-get :jumpclocked)))
      :transient t)]]
  "Casual Agenda navigation group within an Agenda.")


(defun casual-agenda-goto-now ()
  "Redo agenda view and move point to current time \"now\"."
  (interactive)
  (org-agenda-redo)
  (org-agenda-goto-today)
  (search-forward org-agenda-current-time-string)
  (beginning-of-line))

(defun casual-agenda-clock-in (&optional select start-time)
  "Start clock optionally with SELECT, START-TIME.

This is a wrapper around `org-clock-in'. Upon successful
completion, `casual-agenda-goto-now' is called."
  (interactive "P")
  (ignore select)
  (ignore start-time)
  (let* ((m (org-get-at-bol 'org-marker))
         (local-marker-buffer (marker-buffer m))
         (success nil))
    (if (and (markerp m) local-marker-buffer)
        (with-current-buffer local-marker-buffer
          (call-interactively #'org-clock-in)
          (setq success t))
      (message "Command not supported"))

    (if success
        (casual-agenda-goto-now))))

(defun casual-agenda-clock-out (&optional switch-to-state fail-quietly at-time)
  "Stop running clock optionally with SWITCH-TO-STATE, FAIL-QUIETLY, and AT-TIME.

This is a wrapper around `org-clock-out'. Upon successful
completion, `casual-agenda-goto-now' is called."
  (interactive "P")
  (ignore switch-to-state)
  (ignore fail-quietly)
  (ignore at-time)
  (call-interactively #'org-clock-out)
  (casual-agenda-goto-now))

(defun casual-agenda-clock-cancel ()
  "Cancel running clock.

This is a wrapper around `org-clock-cancel'. Upon successful
completion, `casual-agenda-goto-now' is called."
  (interactive)
  (call-interactively #'org-clock-cancel)
  (casual-agenda-goto-now))

(defun casual-agenda-headlinep ()
  "Predicate to determine if the point is on a headline in an agenda view."
  (let ((m (org-get-at-bol 'org-marker)))
    (and (markerp m) (marker-buffer m))))

(defun casual-agenda-type-agendap ()
  "Predicate to determine if the point is on an agenda view."
  (memq org-agenda-type '(agenda)))

(defun casual-agenda-type-datep ()
  "Predicate to determine if on date."
  (and (casual-agenda-type-agendap)
       (get-text-property (min (1- (point-max)) (point)) 'day)))

(provide 'casual-agenda-utils)
;;; casual-agenda-utils.el ends here
