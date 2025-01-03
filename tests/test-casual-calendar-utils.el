;;; test-casual-calendar-utils.el --- Casual Calendar Utils Tests  -*- lexical-binding: t; -*-

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
(require 'casual-calendar-test-utils)
(require 'casual-calendar-utils)

(ert-deftest test-casual-calendar-diary-and-goto-tmenu ()
  (let ()
    (casualt-calendar-setup)
    (cl-letf ((casualt-mock #'diary-insert-entry)
              (casualt-mock #'diary-insert-weekly-entry)
              (casualt-mock #'diary-insert-monthly-entry)
              (casualt-mock #'diary-insert-yearly-entry)
              (casualt-mock #'diary-insert-anniversary-entry)
              (casualt-mock #'calendar-goto-date)
              (casualt-mock #'calendar-iso-goto-date)
              (casualt-mock #'calendar-goto-day-of-year))

      (let ((test-vectors
             '((:binding "e" :command diary-insert-entry)
               (:binding "w" :command diary-insert-weekly-entry)
               (:binding "m" :command diary-insert-monthly-entry)
               (:binding "y" :command diary-insert-yearly-entry)
               (:binding "a" :command diary-insert-anniversary-entry)
               (:binding "g" :command calendar-goto-date)
               ;; (:binding "i" :command calendar-iso-goto-date) ; TODO: need to mock input
               (:binding "d" :command calendar-goto-day-of-year))))

        (casualt-suffix-testcase-runner test-vectors
                                        #'casual-calendar-diary-and-goto-tmenu
                                        '(lambda () (random 5000)))))
    (casualt-calendar-breakdown t)))

(ert-deftest test-casual-calendar-conversions-tmenu ()
  (let ()
    (casualt-calendar-setup)
    (cl-letf ((casualt-mock #'calendar-print-other-dates))

      (let ((test-vectors
             '((:binding "a" :command casual-calendar-astro-tmenu)
               (:binding "b" :command casual-calendar-bahai-tmenu)
               (:binding "c" :command casual-calendar-coptic-tmenu)
               (:binding "e" :command casual-calendar-ethiopic-tmenu)
               (:binding "f" :command casual-calendar-french-tmenu)
               (:binding "h" :command casual-calendar-hebrew-tmenu)
               (:binding "i" :command casual-calendar-islamic-tmenu)
               (:binding "j" :command casual-calendar-julian-tmenu)
               (:binding "l" :command casual-calendar-lunar-tmenu)
               (:binding "m" :command casual-calendar-mayan-tmenu)
               (:binding "p" :command casual-calendar-persian-tmenu)
               (:binding "A" :command calendar-print-other-dates))))

        (casualt-suffix-testcase-runner test-vectors
                                        #'casual-calendar-conversions-tmenu
                                        '(lambda () (random 5000)))))
    (casualt-calendar-breakdown)))

(ert-deftest test-casual-calendar-lunar-tmenu ()
  (let ()
    (casualt-calendar-setup)
    (cl-letf ((casualt-mock #'calendar-chinese-print-date)
              (casualt-mock #'calendar-chinese-goto-date)
              (casualt-mock #'diary-chinese-insert-entry)
              (casualt-mock #'diary-chinese-insert-monthly-entry)
              (casualt-mock #'diary-chinese-insert-yearly-entry)
              (casualt-mock #'diary-chinese-insert-anniversary-entry)
              (casualt-mock #'diary-view-entries)
              (casualt-mock #'diary-show-all-entries))

      (let ((test-vectors
             '((:binding "c" :command calendar-chinese-print-date)
               ;;(:binding "G" :command calendar-chinese-goto-date)
               (:binding "i" :command diary-chinese-insert-entry)
               (:binding "m" :command diary-chinese-insert-monthly-entry)
               (:binding "y" :command diary-chinese-insert-yearly-entry)
               (:binding "A" :command diary-chinese-insert-anniversary-entry)
               (:binding "d" :command diary-view-entries)
               (:binding "s" :command diary-show-all-entries))))

        (casualt-suffix-testcase-runner test-vectors
                                        #'casual-calendar-lunar-tmenu
                                        '(lambda () (random 5000)))))
    (casualt-calendar-breakdown)))

(ert-deftest test-casual-calendar-astro-tmenu ()
  (let ()
    (casualt-calendar-setup)
    (cl-letf ((casualt-mock #'calendar-astro-print-day-number)
              (casualt-mock #'calendar-astro-goto-day-number)
              (casualt-mock #'diary-view-entries)
              (casualt-mock #'diary-show-all-entries))

      (let ((test-vectors
             '((:binding "c" :command calendar-astro-print-day-number)
               (:binding "d" :command diary-view-entries)
               (:binding "s" :command diary-show-all-entries))))

        (casualt-suffix-testcase-runner test-vectors
                                        #'casual-calendar-astro-tmenu
                                        '(lambda () (random 5000)))))
    (casualt-calendar-breakdown)))

(ert-deftest test-casual-calendar-islamic-tmenu ()
  (let ()
    (casualt-calendar-setup)
    (cl-letf ((casualt-mock #'calendar-islamic-print-day-number)
              (casualt-mock #'calendar-islamic-goto-day-number)
              (casualt-mock #'diary-islamic-insert-entry)
              (casualt-mock #'diary-islamic-insert-monthly-entry)
              (casualt-mock #'diary-islamic-insert-yearly-entry)
              (casualt-mock #'diary-view-entries)
              (casualt-mock #'diary-show-all-entries))

      (let ((test-vectors
             '((:binding "c" :command calendar-islamic-print-date)
               ;;(:binding "G" :command calendar-islamic-goto-date)
               (:binding "i" :command diary-islamic-insert-entry)
               (:binding "m" :command diary-islamic-insert-monthly-entry)
               (:binding "y" :command diary-islamic-insert-yearly-entry)
               (:binding "d" :command diary-view-entries)
               (:binding "s" :command diary-show-all-entries))))

        (casualt-suffix-testcase-runner test-vectors
                                        #'casual-calendar-islamic-tmenu
                                        '(lambda () (random 5000)))))
    (casualt-calendar-breakdown)))

(ert-deftest test-casual-calendar-hebrew-tmenu ()
  (let ()
    (casualt-calendar-setup)
    (cl-letf ((casualt-mock #'calendar-hebrew-print-day-number)
              (casualt-mock #'calendar-hebrew-goto-day-number)
              (casualt-mock #'diary-hebrew-insert-entry)
              (casualt-mock #'diary-hebrew-insert-monthly-entry)
              (casualt-mock #'diary-hebrew-insert-yearly-entry)
              (casualt-mock #'diary-view-entries)
              (casualt-mock #'diary-show-all-entries))

      (let ((test-vectors
             '((:binding "c" :command calendar-hebrew-print-date)
               ;;(:binding "G" :command calendar-hebrew-goto-date)
               (:binding "i" :command diary-hebrew-insert-entry)
               (:binding "m" :command diary-hebrew-insert-monthly-entry)
               (:binding "y" :command diary-hebrew-insert-yearly-entry)
               (:binding "d" :command diary-view-entries)
               (:binding "s" :command diary-show-all-entries))))

        (casualt-suffix-testcase-runner test-vectors
                                        #'casual-calendar-hebrew-tmenu
                                        '(lambda () (random 5000)))))
    (casualt-calendar-breakdown)))

(ert-deftest test-casual-calendar-bahai-tmenu ()
  (let ()
    (casualt-calendar-setup)
    (cl-letf ((casualt-mock #'calendar-bahai-print-day-number)
              (casualt-mock #'calendar-bahai-goto-day-number)
              (casualt-mock #'diary-bahai-insert-entry)
              (casualt-mock #'diary-bahai-insert-monthly-entry)
              (casualt-mock #'diary-bahai-insert-yearly-entry)
              (casualt-mock #'diary-view-entries)
              (casualt-mock #'diary-show-all-entries))

      (let ((test-vectors
             '((:binding "c" :command calendar-bahai-print-date)
               ;;(:binding "G" :command calendar-bahai-goto-date)
               (:binding "i" :command diary-bahai-insert-entry)
               (:binding "m" :command diary-bahai-insert-monthly-entry)
               (:binding "y" :command diary-bahai-insert-yearly-entry)
               (:binding "d" :command diary-view-entries)
               (:binding "s" :command diary-show-all-entries))))

        (casualt-suffix-testcase-runner test-vectors
                                        #'casual-calendar-bahai-tmenu
                                        '(lambda () (random 5000)))))
    (casualt-calendar-breakdown)))

(ert-deftest test-casual-calendar-ethiopic-tmenu ()
  (let ()
    (casualt-calendar-setup)
    (cl-letf ((casualt-mock #'calendar-ethiopic-print-day-number)
              (casualt-mock #'calendar-ethiopic-goto-day-number)
              (casualt-mock #'diary-view-entries)
              (casualt-mock #'diary-show-all-entries))

      (let ((test-vectors
             '((:binding "c" :command calendar-ethiopic-print-date)
               ;;(:binding "G" :command calendar-ethiopic-goto-date)
               (:binding "d" :command diary-view-entries)
               (:binding "s" :command diary-show-all-entries))))

        (casualt-suffix-testcase-runner test-vectors
                                        #'casual-calendar-ethiopic-tmenu
                                        '(lambda () (random 5000)))))
    (casualt-calendar-breakdown)))

(ert-deftest test-casual-calendar-french-tmenu ()
  (let ()
    (casualt-calendar-setup)
    (cl-letf ((casualt-mock #'calendar-french-print-day-number)
              (casualt-mock #'calendar-french-goto-day-number)
              (casualt-mock #'diary-view-entries)
              (casualt-mock #'diary-show-all-entries))

      (let ((test-vectors
             '((:binding "c" :command calendar-french-print-date)
               ;;(:binding "G" :command calendar-french-goto-date)
               (:binding "d" :command diary-view-entries)
               (:binding "s" :command diary-show-all-entries))))

        (casualt-suffix-testcase-runner test-vectors
                                        #'casual-calendar-french-tmenu
                                        '(lambda () (random 5000)))))
    (casualt-calendar-breakdown)))

(ert-deftest test-casual-calendar-julian-tmenu ()
  (let ()
    (casualt-calendar-setup)
    (cl-letf ((casualt-mock #'calendar-julian-print-day-number)
              (casualt-mock #'calendar-julian-goto-day-number)
              (casualt-mock #'diary-view-entries)
              (casualt-mock #'diary-show-all-entries))

      (let ((test-vectors
             '((:binding "c" :command calendar-julian-print-date)
               ;;(:binding "G" :command calendar-julian-goto-date)
               (:binding "d" :command diary-view-entries)
               (:binding "s" :command diary-show-all-entries))))

        (casualt-suffix-testcase-runner test-vectors
                                        #'casual-calendar-julian-tmenu
                                        '(lambda () (random 5000)))))
    (casualt-calendar-breakdown)))

(ert-deftest test-casual-calendar-coptic-tmenu ()
  (let ()
    (casualt-calendar-setup)
    (cl-letf ((casualt-mock #'calendar-coptic-print-day-number)
              (casualt-mock #'calendar-coptic-goto-day-number)
              (casualt-mock #'diary-view-entries)
              (casualt-mock #'diary-show-all-entries))

      (let ((test-vectors
             '((:binding "c" :command calendar-coptic-print-date)
               ;;(:binding "G" :command calendar-coptic-goto-date)
               (:binding "d" :command diary-view-entries)
               (:binding "s" :command diary-show-all-entries))))

        (casualt-suffix-testcase-runner test-vectors
                                        #'casual-calendar-coptic-tmenu
                                        '(lambda () (random 5000)))))
    (casualt-calendar-breakdown)))

(ert-deftest test-casual-calendar-persian-tmenu ()
  (let ()
    (casualt-calendar-setup)
    (cl-letf ((casualt-mock #'calendar-persian-print-day-number)
              (casualt-mock #'calendar-persian-goto-day-number)
              (casualt-mock #'diary-view-entries)
              (casualt-mock #'diary-show-all-entries))

      (let ((test-vectors
             '((:binding "c" :command calendar-persian-print-date)
               ;;(:binding "G" :command calendar-persian-goto-date)
               (:binding "d" :command diary-view-entries)
               (:binding "s" :command diary-show-all-entries))))

        (casualt-suffix-testcase-runner test-vectors
                                        #'casual-calendar-persian-tmenu
                                        '(lambda () (random 5000)))))
    (casualt-calendar-breakdown)))

(ert-deftest test-casual-calendar-mayan-tmenu ()
  (let ()
    (casualt-calendar-setup)
    (cl-letf ((casualt-mock #'calendar-mayan-print-day-number)
              (casualt-mock #'calendar-mayan-goto-day-number)
              (casualt-mock #'diary-view-entries)
              (casualt-mock #'diary-show-all-entries))

      (let ((test-vectors
             '((:binding "c" :command calendar-mayan-print-date)
               ;;(:binding "G" :command calendar-mayan-goto-date)
               (:binding "d" :command diary-view-entries)
               (:binding "s" :command diary-show-all-entries))))

        (casualt-suffix-testcase-runner test-vectors
                                        #'casual-calendar-mayan-tmenu
                                        '(lambda () (random 5000)))))
    (casualt-calendar-breakdown)))

(provide 'test-casual-calendar-utils)
;;; test-casual-calendar-utils.el ends here
