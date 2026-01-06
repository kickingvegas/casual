;;; test-casual-agenda.el --- Casual Agenda Tests -*- lexical-binding: t; -*-

;; Copyright (C) 2024-2025  Charles Y. Choi

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
(require 'casual-agenda-test-utils)
(require 'casual-lib-test-utils)
(require 'casual-agenda)


(ert-deftest test-casual-agenda-tmenu ()
  (casualt-agenda-setup)
  (cl-letf (((symbol-function #'casual-agenda-headlinep) (lambda () t))
            ((symbol-function #'casual-agenda-type-datep) (lambda () t))
            ((symbol-function #'casual-agenda-type-agendap) (lambda () t))
            ((symbol-function #'org-clocking-p) (lambda () t))
            (casualt-mock #'org-agenda-previous-line)
            (casualt-mock #'org-agenda-next-line)
            (casualt-mock #'org-agenda-previous-item)
            (casualt-mock #'org-agenda-next-item)
            (casualt-mock #'org-agenda-previous-date-line)
            (casualt-mock #'org-agenda-next-date-line)
            (casualt-mock #'org-agenda-earlier)
            (casualt-mock #'org-agenda-later)
            (casualt-mock #'org-agenda-redo)
            (casualt-mock #'org-agenda-redo-all)
            (casualt-mock #'org-agenda-goto-date)
            (casualt-mock #'org-agenda-clock-goto)

            (casualt-mock #'org-agenda-filter)
            (casualt-mock #'org-agenda-filter-by-regexp)
            (casualt-mock #'org-agenda-filter-by-tag)
            (casualt-mock #'org-agenda-filter-by-top-headline)
            (casualt-mock #'org-agenda-filter-by-category)
            (casualt-mock #'org-agenda-filter-by-effort)
            (casualt-mock #'org-agenda-filter-remove-all)

            (casualt-mock #'org-agenda-day-view)
            (casualt-mock #'org-agenda-week-view)
            (casualt-mock #'org-agenda-month-view)
            (casualt-mock #'org-agenda-fortnight-view)
            (casualt-mock #'org-agenda-year-view)
            (casualt-mock #'org-save-all-org-buffers)
            (casualt-mock #'org-capture)
            (casualt-mock #'org-agenda)
            (casualt-mock #'bookmark-jump)

            (casualt-mock #'org-agenda-goto-calendar)
            (casualt-mock #'org-agenda-sunrise-sunset)
            (casualt-mock #'org-agenda-phases-of-moon)
            (casualt-mock #'org-agenda-holidays)
            (casualt-mock #'org-agenda-undo)
            (casualt-mock #'org-info-find-node)
            (casualt-mock #'org-timer-set-timer)
            (casualt-mock #'org-agenda-quit))

    (let ((test-vectors
           '((:binding "p" :command org-agenda-previous-line)
             (:binding "n" :command org-agenda-next-line)
             (:binding "P" :command org-agenda-previous-item)
             (:binding "N" :command org-agenda-next-item)
             (:binding "M-p" :command org-agenda-previous-date-line)
             (:binding "M-n" :command org-agenda-next-date-line)
             (:binding "b" :command org-agenda-earlier)
             (:binding "f" :command org-agenda-later)
             (:binding "r" :command org-agenda-redo)
             (:binding "g" :command org-agenda-redo-all)
             (:binding "j" :command org-agenda-goto-date)
             (:binding "M-j" :command org-agenda-clock-goto)

             (:binding "/" :command org-agenda-filter)
             (:binding "=" :command org-agenda-filter-by-regexp)
             (:binding "\\" :command org-agenda-filter-by-tag)
             (:binding "^" :command org-agenda-filter-by-top-headline)
             (:binding "<" :command org-agenda-filter-by-category)
             (:binding "_" :command org-agenda-filter-by-effort)
             (:binding "|" :command org-agenda-filter-remove-all)

             (:binding "d" :command org-agenda-day-view)
             (:binding "w" :command org-agenda-week-view)
             (:binding "m" :command org-agenda-month-view)
             (:binding "y" :command org-agenda-year-view)
             (:binding "o" :command casual-agenda-operations-tmenu)
             (:binding "M" :command casual-agenda-mark-tmenu)

             (:binding "s" :command org-save-all-org-buffers)
             (:binding "k" :command org-capture)
             (:binding "a" :command org-agenda)
             (:binding "J" :command bookmark-jump)

             (:binding ";" :command org-timer-set-timer)
             (:binding "c" :command org-agenda-goto-calendar)
             (:binding "l" :command casual-agenda-almanac-tmenu)

             ;;(:binding "" :command org-agenda-undo)
             (:binding "I" :command org-info-find-node)

             (:binding "," :command casual-agenda-settings-tmenu)
             (:binding "q" :command org-agenda-quit))))

      (casualt-suffix-testcase-runner test-vectors
                                      #'casual-agenda-tmenu
                                      '(lambda () (random 5000)))))
  (casualt-agenda-breakdown t))

(ert-deftest test-casual-agenda-almanac-tmenu ()
  (casualt-agenda-setup)
  (cl-letf (((symbol-function #'casual-agenda-headlinep) (lambda () t))
            ((symbol-function #'casual-agenda-type-datep) (lambda () t))
            ((symbol-function #'casual-agenda-type-agendap) (lambda () t))
            ((symbol-function #'org-clocking-p) (lambda () t))
            (casualt-mock #'org-agenda-sunrise-sunset)
            (casualt-mock #'org-agenda-phases-of-moon)
            (casualt-mock #'org-agenda-holidays)

            (casualt-mock #'org-agenda-previous-line)
            (casualt-mock #'org-agenda-next-line)
            (casualt-mock #'org-agenda-previous-item)
            (casualt-mock #'org-agenda-next-item)
            (casualt-mock #'org-agenda-previous-date-line)
            (casualt-mock #'org-agenda-next-date-line)
            (casualt-mock #'org-agenda-earlier)
            (casualt-mock #'org-agenda-later)
            (casualt-mock #'org-agenda-redo)
            (casualt-mock #'org-agenda-redo-all)
            (casualt-mock #'org-agenda-goto-date)
            (casualt-mock #'org-agenda-clock-goto))

    (let ((test-vectors
           '((:binding "S" :command org-agenda-sunrise-sunset)
             (:binding "M" :command org-agenda-phases-of-moon)
             (:binding "H" :command org-agenda-holidays)

             (:binding "p" :command org-agenda-previous-line)
             (:binding "n" :command org-agenda-next-line)
             (:binding "P" :command org-agenda-previous-item)
             (:binding "N" :command org-agenda-next-item)
             (:binding "M-p" :command org-agenda-previous-date-line)
             (:binding "M-n" :command org-agenda-next-date-line)
             (:binding "b" :command org-agenda-earlier)
             (:binding "f" :command org-agenda-later)
             (:binding "r" :command org-agenda-redo)
             (:binding "g" :command org-agenda-redo-all)
             (:binding "j" :command org-agenda-goto-date)
             (:binding "M-j" :command org-agenda-clock-goto))))

      (casualt-suffix-testcase-runner test-vectors
                                      #'casual-agenda-almanac-tmenu
                                      '(lambda () (random 5000)))))
  (casualt-agenda-breakdown t t))



(ert-deftest test-casual-agenda-operations-tmenu ()
  (casualt-agenda-setup)
  (cl-letf (((symbol-function #'casual-agenda-headlinep) (lambda () t))
            ((symbol-function #'casual-agenda-type-datep) (lambda () t))
            ((symbol-function #'casual-agenda-type-agendap) (lambda () t))
            ((symbol-function #'org-clocking-p) (lambda () t))
            (casualt-mock #'org-agenda-todo)
            (casualt-mock #'org-agenda-schedule)
            (casualt-mock #'org-agenda-deadline)
            (casualt-mock #'org-agenda-set-tags)
            (casualt-mock #'org-agenda-priority-up)
            (casualt-mock #'org-agenda-priority-down)
            (casualt-mock #'org-agenda-refile)
            (casualt-mock #'org-agenda-add-note)
            (casualt-mock #'org-agenda-archive-default-with-confirmation)

            (casualt-mock #'org-clock-cancel)
            (casualt-mock #'org-clock-modify-effort-estimate)
            (casualt-mock #'casual-agenda-clock-out)

            (casualt-mock #'org-agenda-previous-line)
            (casualt-mock #'org-agenda-next-line)
            (casualt-mock #'org-agenda-previous-item)
            (casualt-mock #'org-agenda-next-item)
            (casualt-mock #'org-agenda-previous-date-line)
            (casualt-mock #'org-agenda-next-date-line)
            (casualt-mock #'org-agenda-earlier)
            (casualt-mock #'org-agenda-later)
            (casualt-mock #'org-agenda-redo)
            (casualt-mock #'org-agenda-redo-all)
            (casualt-mock #'org-agenda-goto-date)
            (casualt-mock #'org-agenda-clock-goto)
            (casualt-mock #'org-agenda-set-property)
            (casualt-mock #'org-agenda-bulk-action))

    (let ((test-vectors
           '((:binding "t" :command org-agenda-todo)
             (:binding "B" :command org-agenda-bulk-action)
             (:binding "s" :command org-agenda-schedule)
             (:binding "d" :command org-agenda-deadline)
             (:binding ":" :command org-agenda-set-tags)
             (:binding "+" :command org-agenda-priority-up)
             (:binding "-" :command org-agenda-priority-down)
             (:binding "R" :command org-agenda-refile)
             (:binding "z" :command org-agenda-add-note)
             (:binding "S" :command org-agenda-set-property)
             (:binding "A" :command org-agenda-archive-default-with-confirmation)

             (:binding "I" :command casual-agenda-clock-in)

             ;; TODO: figure out how to test :inapt switches
             (:binding "O" :command casual-agenda-clock-out)
             (:binding "x" :command casual-agenda-clock-cancel)
             (:binding "m" :command org-clock-modify-effort-estimate)

             (:binding "p" :command org-agenda-previous-line)
             (:binding "n" :command org-agenda-next-line)
             (:binding "P" :command org-agenda-previous-item)
             (:binding "N" :command org-agenda-next-item)
             (:binding "M-p" :command org-agenda-previous-date-line)
             (:binding "M-n" :command org-agenda-next-date-line)
             (:binding "b" :command org-agenda-earlier)
             (:binding "f" :command org-agenda-later)
             (:binding "r" :command org-agenda-redo)
             (:binding "g" :command org-agenda-redo-all)
             (:binding "j" :command org-agenda-goto-date)
             (:binding "M-j" :command org-agenda-clock-goto))))

      (defun org-clocking-p ()
        t)

      (casualt-suffix-testcase-runner test-vectors
                                      #'casual-agenda-operations-tmenu
                                      '(lambda () (random 5000)))))
  (casualt-agenda-breakdown t t))


(ert-deftest test-casual-agenda-mark-tmenu ()
  (casualt-agenda-setup)
  (cl-letf (((symbol-function #'casual-agenda-headlinep) (lambda () t))
            ((symbol-function #'casual-agenda-type-datep) (lambda () t))
            ((symbol-function #'casual-agenda-type-agendap) (lambda () t))
            ((symbol-function #'org-clocking-p) (lambda () t))
            (casualt-mock #'org-agenda-bulk-mark)
            (casualt-mock #'org-agenda-bulk-mark-regexp)
            (casualt-mock #'org-agenda-bulk-unmark)
            (casualt-mock #'org-agenda-bulk-unmark-all)
            (casualt-mock #'org-agenda-bulk-toggle)
            (casualt-mock #'org-agenda-bulk-toggle-all)
            (casualt-mock #'org-agenda-bulk-action)

            (casualt-mock #'org-agenda-previous-line)
            (casualt-mock #'org-agenda-next-line)
            (casualt-mock #'org-agenda-previous-item)
            (casualt-mock #'org-agenda-next-item)
            (casualt-mock #'org-agenda-previous-date-line)
            (casualt-mock #'org-agenda-next-date-line)
            (casualt-mock #'org-agenda-earlier)
            (casualt-mock #'org-agenda-later)
            (casualt-mock #'org-agenda-redo)
            (casualt-mock #'org-agenda-redo-all)
            (casualt-mock #'org-agenda-goto-date)
            (casualt-mock #'org-agenda-clock-goto))

    (let ((test-vectors
           '((:binding "m" :command org-agenda-bulk-mark)
             (:binding "x" :command org-agenda-bulk-mark-regexp)
             (:binding "u" :command org-agenda-bulk-unmark)
             (:binding "U" :command org-agenda-bulk-unmark-all)
             (:binding "t" :command org-agenda-bulk-toggle)
             (:binding "T" :command org-agenda-bulk-toggle-all)
             (:binding "B" :command org-agenda-bulk-action)

             (:binding "p" :command org-agenda-previous-line)
             (:binding "n" :command org-agenda-next-line)
             (:binding "P" :command org-agenda-previous-item)
             (:binding "N" :command org-agenda-next-item)
             (:binding "M-p" :command org-agenda-previous-date-line)
             (:binding "M-n" :command org-agenda-next-date-line)
             (:binding "b" :command org-agenda-earlier)
             (:binding "f" :command org-agenda-later)
             (:binding "r" :command org-agenda-redo)
             (:binding "g" :command org-agenda-redo-all)
             (:binding "j" :command org-agenda-goto-date)
             (:binding "M-j" :command org-agenda-clock-goto))))

      (casualt-suffix-testcase-runner test-vectors
                                      #'casual-agenda-mark-tmenu
                                      '(lambda () (random 5000)))))
  (casualt-agenda-breakdown  t t))

(provide 'test-casual-agenda)
;;; test-casual-agenda.el ends here
