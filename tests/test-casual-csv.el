;;; test-casual-csv.el --- Casual Make Tests -*- lexical-binding: t; -*-

;; Copyright (C) 2025  Charles Y. Choi

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
(require 'casual-csv-test-utils)
(require 'casual-lib-test-utils)
(require 'casual-csv)

(ert-deftest test-casual-csv-tmenu ()
  (let ()
    (casualt-csv-setup)

    (cl-letf (
              (casualt-mock #'csv-backtab-command)
              (casualt-mock #'csv-tab-command)
              (casualt-mock #'previous-line)
              (casualt-mock #'next-line)
              (casualt-mock #'move-beginning-of-line)
              (casualt-mock #'move-end-of-line)
              (casualt-mock #'beginning-of-buffer)
              (casualt-mock #'end-of-buffer)
              (casualt-mock #'scroll-down-command)
              (casualt-mock #'scroll-up-command)
              (casualt-mock #'casual-csv-align-tmenu)
              (casualt-mock #'view-mode)
              (casualt-mock #'View-exit)
              (casualt-mock #'casual-lib-duplicate-file)
              (casualt-mock #'mark-sexp)
              (casualt-mock #'casual-editkit-copy-sexp)

              (casualt-mock #'occur)
              (casualt-mock #'casual-csv-kill-region-as-org-table)
              (casualt-mock #'casual-csv-settings-tmenu)

              (casualt-mock #'quit-window))

      (let ((test-vectors
             '(
               (:binding "S-TAB" :command csv-backtab-command)
               (:binding "TAB" :command csv-tab-command)
               (:binding "n" :command next-line)
               (:binding "p" :command previous-line)
               (:binding "C-e" :command move-end-of-line)
               (:binding "C-a" :command move-beginning-of-line)
               (:binding ">" :command end-of-buffer)
               (:binding "<" :command beginning-of-buffer)
               (:binding "C-v" :command scroll-up-command)
               (:binding "M-v" :command scroll-down-command)
               (:binding "a" :command casual-csv-align-tmenu)
               ;; (:binding "v" :command view-mode)
               ;; (:binding "e" :command View-exit)
               ;; (:binding "s" :command csv-sort-fields)

               (:binding "m" :command mark-sexp)
               (:binding "c" :command casual-editkit-copy-sexp)

               (:binding "o" :command occur)
               (:binding "," :command casual-csv-settings-tmenu)
               (:binding "q" :command quit-window)
               )))

        (casualt-suffix-testcase-runner test-vectors
                                        #'casual-csv-tmenu
                                        '(lambda () (random 5000)))))
    (casualt-csv-breakdown)))

(provide 'test-casual-csv)
;;; test-casual-csv.el ends here
