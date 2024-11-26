;;; test-casual-calendar.el --- Casual Calendar Tests -*- lexical-binding: t; -*-

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

;;

;;; Code:

(require 'ert)
(require 'casual-lib-test-utils)
(require 'casual-calendar-test-utils)
(require 'casual-calendar)

(ert-deftest test-casual-calendar-tmenu ()
  (let ()
    (casualt-calendar-setup)

    (cl-letf (;; ((symbol-function #'buffer-file-name) (lambda () t))
              (casualt-mock #'calendar-backward-day)
              (casualt-mock #'calendar-forward-day)
              (casualt-mock #'calendar-goto-today)
              (casualt-mock #'calendar-goto-date)

              (casualt-mock #'calendar-backward-week)
              (casualt-mock #'calendar-forward-week)
              (casualt-mock #'calendar-beginning-of-week)
              (casualt-mock #'calendar-end-of-week)
              (casualt-mock #'calendar-iso-goto-week)
              (casualt-mock #'calendar-iso-print-date)

              (casualt-mock #'calendar-backward-month)
              (casualt-mock #'calendar-forward-month)
              (casualt-mock #'calendar-beginning-of-month)
              (casualt-mock #'calendar-end-of-month)
              (casualt-mock #'calendar-other-month)

              (casualt-mock #'calendar-backward-year)
              (casualt-mock #'calendar-forward-year)
              (casualt-mock #'calendar-beginning-of-year)
              (casualt-mock #'calendar-end-of-year)

              (casualt-mock #'calendar-scroll-right)
              (casualt-mock #'calendar-scroll-left)
              (casualt-mock #'calendar-scroll-right-three-months)
              (casualt-mock #'calendar-scroll-left-three-months)
              (casualt-mock #'calendar-redraw)

              (casualt-mock #'calendar-print-other-dates)
              (casualt-mock #'calendar-list-holidays)
              (casualt-mock #'calendar-cursor-holidays)
              (casualt-mock #'calendar-mark-holidays)
              (casualt-mock #'calendar-unmark)
              (casualt-mock #'org-calendar-goto-agenda)
              (casualt-mock #'diary-view-entries)
              (casualt-mock #'diary-show-all-entries)
              (casualt-mock #'calendar-lunar-phases)
              (casualt-mock #'calendar-sunrise-sunset)
              (casualt-mock #'calendar-sunrise-sunset-month)
              (casualt-mock #'calendar-set-mark)
              (casualt-mock #'calendar-count-days-region)

              (casualt-mock #'calendar-exit))

      (let ((test-vectors
             '((:binding "b" :command calendar-backward-day)
               (:binding "f" :command calendar-forward-day)
               (:binding "." :command calendar-goto-today)
               (:binding "g" :command calendar-goto-date)

               (:binding "p" :command calendar-backward-week)
               (:binding "n" :command calendar-forward-week)
               (:binding "a" :command calendar-beginning-of-week)
               (:binding "e" :command calendar-end-of-week)
               ;;(:binding "w" :command calendar-iso-goto-week) ; TODO: needs input handling

               (:binding "{" :command calendar-backward-month)
               (:binding "}" :command calendar-forward-month)
               (:binding "M-a" :command calendar-beginning-of-month)
               (:binding "M-e" :command calendar-end-of-month)
               (:binding "o" :command calendar-other-month)

               (:binding "M-]" :command calendar-forward-year)
               (:binding "M-[" :command calendar-backward-year)
               (:binding "[" :command calendar-beginning-of-year)
               (:binding "]" :command calendar-end-of-year)

               (:binding "<" :command calendar-scroll-right)
               (:binding ">" :command calendar-scroll-left)
               (:binding "-" :command calendar-scroll-right-three-months)
               (:binding "+" :command calendar-scroll-left-three-months)
               (:binding "C-l" :command calendar-redraw)

               (:binding "c" :command casual-calendar-conversions-tmenu)
               (:binding "A" :command calendar-print-other-dates)
               (:binding "i" :command calendar-iso-print-date)
               (:binding "H" :command calendar-list-holidays)
               (:binding "h" :command calendar-cursor-holidays)
               (:binding "x" :command calendar-mark-holidays)
               (:binding "u" :command calendar-unmark)
               (:binding "O" :command org-calendar-goto-agenda)
               (:binding "d" :command diary-view-entries)
               (:binding "D" :command casual-calendar-diary-and-goto-tmenu)
               (:binding "s" :command diary-show-all-entries)
               (:binding "M" :command calendar-lunar-phases)
               (:binding "S" :command calendar-sunrise-sunset)
               (:binding "M-m" :command calendar-sunrise-sunset-month)
               (:binding "C-SPC" :command calendar-set-mark)
               (:binding "=" :command calendar-count-days-region))))

        (casualt-suffix-testcase-runner test-vectors
                                        #'casual-calendar-tmenu
                                        '(lambda () (random 5000)))))
    (casualt-calendar-breakdown)))

(provide 'test-casual-calendar)
;;; test-casual-calendar.el ends here
