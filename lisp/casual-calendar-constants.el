;;; casual-calendar-constants.el --- Casual Calendar Constants -*- lexical-binding: t; -*-

;; Copyright (C) 2024-2026 Charles Y. Choi

;; Author: Charles Choi <kickingvegas@gmail.com>
;; Keywords: calendar

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
(require 'calendar)
(require 'casual-lib)

(defconst casual-calendar-unicode-db
  '((:previous . '("↑" "Previous"))
    (:next . '("↓" "Next"))

    (:behind . '("←" "Behind"))
    (:ahead . '("→" "Ahead"))

    (:beginning . '("⇤" "Beginning"))
    (:end . '("⇥" "End"))

    (:back-3-months . '("← 3 months" "-3 months"))
    (:forward-3-months . '("→ 3 months" "+3 months"))

    (:redraw . '("⟳" "Refresh"))
    ;;(:goto . '("🚀" "Goto"))
    (:goto . '("🔎" "Goto"))
    (:sunrise . '("🌅" "Sunrise"))
    (:lunar . '("🌙" "Lunar")))

  "Unicode symbol DB to use for Calendar Transient menus.")

(defun casual-calendar-unicode-get (key)
  "Lookup Unicode symbol for KEY in DB.

- KEY symbol used to lookup Unicode symbol in DB.

If the value of customizable variable `casual-lib-use-unicode'
is non-nil, then the Unicode symbol is returned, otherwise a
plain ASCII-range string."
  (casual-lib-unicode-db-get key casual-calendar-unicode-db))

;; Transient navigation group for calendar.
(transient-define-group casual-calendar--navigation-group
  ["Navigation"
   ["Day"
    ("b" "Behind" calendar-backward-day
     :description (lambda () (casual-calendar-unicode-get :behind))
     :transient t)
    ("f" "Ahead" calendar-forward-day
     :description (lambda () (casual-calendar-unicode-get :ahead))
     :transient t)
    ("." "Today" calendar-goto-today :transient t)
    ("g" "Goto…" calendar-goto-date
     ;; :description (lambda () (format "%s…" (casual-calendar-unicode-get :goto)))
     :transient t)]
   ["Week"
    ("p" "Behind" calendar-backward-week
     :description (lambda () (casual-calendar-unicode-get :behind))
     :transient t)
    ("n" "Ahead" calendar-forward-week
     :description (lambda () (casual-calendar-unicode-get :ahead))
     :transient t)
    ("a" "Beginning" calendar-beginning-of-week
     :description (lambda () (casual-calendar-unicode-get :beginning))
     :transient t)
    ("e" "End" calendar-end-of-week
     :description (lambda () (casual-calendar-unicode-get :end))
     :transient t)
    ("w" "Goto…" calendar-iso-goto-week
     ;; :description (lambda () (format "%s…" (casual-calendar-unicode-get :goto)))
     :transient t)]

   ["Month"
    :pad-keys t
    ("{" "Behind" calendar-backward-month
     :description (lambda () (casual-calendar-unicode-get :behind))
     :transient t)
    ("}" "Ahead" calendar-forward-month
     :description (lambda () (casual-calendar-unicode-get :ahead))
     :transient t)
    ("M-a" "Beginning" calendar-beginning-of-month
     :description (lambda () (casual-calendar-unicode-get :beginning))
     :transient t)
    ("M-e" "End" calendar-end-of-month
     :description (lambda () (casual-calendar-unicode-get :end))
     :transient t)
    ("o" "Goto…" calendar-other-month
     ;; :description (lambda () (format "%s…" (casual-calendar-unicode-get :goto)))
     :transient t)]

   ["Year"
    :pad-keys t
    ("M-[" "Behind" calendar-backward-year
     :description (lambda () (casual-calendar-unicode-get :behind))
     :transient t)
    ("M-]" "Ahead" calendar-forward-year
     :description (lambda () (casual-calendar-unicode-get :ahead))
     :transient t)
    ("[" "Beginning" calendar-beginning-of-year
     :description (lambda () (casual-calendar-unicode-get :beginning))
     :transient t)
    ("]" "End" calendar-end-of-year
     :description (lambda () (casual-calendar-unicode-get :end))
     :transient t)]

   ["Scroll"
    :pad-keys t
    ("<" "Behind" calendar-scroll-right
     :description (lambda () (casual-calendar-unicode-get :behind))
     :transient t)
    (">" "Ahead" calendar-scroll-left
     :description (lambda () (casual-calendar-unicode-get :ahead))
     :transient t)
    ("-" "-3 months" calendar-scroll-right-three-months
     :description (lambda () (casual-calendar-unicode-get :back-3-months))
     :transient t)
    ("+" "+3 months" calendar-scroll-left-three-months
     :description (lambda () (casual-calendar-unicode-get :forward-3-months))
     :transient t)
    ("C-l" "Redraw" calendar-redraw
     :description (lambda () (casual-calendar-unicode-get :redraw))
     :transient t)]])

(provide 'casual-calendar-constants)
;;; casual-calendar-constants.el ends here
