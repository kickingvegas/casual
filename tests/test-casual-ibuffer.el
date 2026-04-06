;;; test-casual-ibuffer.el --- Casual IBuffer Tests -*- lexical-binding: t; -*-

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

;;

;;; Code:

(require 'ert)
(require 'casual-ibuffer-test-utils)
(require 'casual-lib-test-utils)
(require 'casual-ibuffer)

(ert-deftest test-casual-ibuffer-palette-tmenu ()
  (let ((tmpfile "casual-ibuffer-palette-tmenu.txt"))
    (casualt-ibuffer-setup)
    (cl-letf ((casualt-mock #'ibuffer-visit-buffer-other-window)
              (casualt-mock #'ibuffer-do-delete)
              (casualt-mock #'ibuffer-do-save)
              (casualt-mock #'ibuffer-diff-with-file)
              (casualt-mock #'ibuffer-copy-filename-as-kill)
              (casualt-mock #'casual-ibuffer-operations-tmenu)
              (casualt-mock #'ibuffer-mark-forward)
              (casualt-mock #'casual-ibuffer-mark-tmenu)
              (casualt-mock #'casual-ibuffer-mark-regexp-tmenu)
              (casualt-mock #'ibuffer-unmark-forward)
              (casualt-mock #'ibuffer-mark-for-delete)
              (casualt-mock #'ibuffer-do-kill-on-deletion-marks)
              (casualt-mock #'ibuffer-unmark-all-marks)

              (casualt-mock #'casual-ibuffer-sortby-tmenu)
              (casualt-mock #'ibuffer-switch-format)
              (casualt-mock #'ibuffer-bury-buffer)
              (casualt-mock #'ibuffer-update)
              (casualt-mock #'ibuffer-toggle-filter-group)

              (casualt-mock #'ibuffer-backward-line)
              (casualt-mock #'ibuffer-forward-line)
              (casualt-mock #'ibuffer-backwards-next-marked)
              (casualt-mock #'ibuffer-forward-next-marked)
              (casualt-mock #'ibuffer-backward-filter-group)
              (casualt-mock #'ibuffer-forward-filter-group)
              (casualt-mock #'ibuffer-jump-to-buffer)
              (casualt-mock #'ibuffer-jump-to-filter-group)

              (casualt-mock #'ibuffer-filter-chosen-by-completion)
              (casualt-mock #'ibuffer-filter-disable)
              (casualt-mock #'casual-ibuffer-filter-tmenu)

              (casualt-mock #'ibuffer-do-isearch)
              (casualt-mock #'ibuffer-do-isearch-regexp)
              (casualt-mock #'ibuffer-do-occur)
              (casualt-mock #'ibuffer-do-query-replace)
              (casualt-mock #'ibuffer-do-query-replace-regexp)

              (casualt-mock #'casual-ibuffer-return-dwim)
              (casualt-mock #'casual-ibuffer-settings-tmenu)
              (casualt-mock #'bookmark-jump)
              (casualt-mock #'quit-window))

      (let ((test-vectors
             '((:binding "o" :command ibuffer-visit-buffer-other-window)
               (:binding "S" :command ibuffer-do-save)
               (:binding "D" :command ibuffer-do-delete)
               (:binding "=" :command ibuffer-diff-with-file)
               (:binding "w" :command ibuffer-copy-filename-as-kill)
               (:binding "M" :command casual-ibuffer-operations-tmenu)

               (:binding "m" :command ibuffer-mark-forward)
               (:binding "t" :command casual-ibuffer-mark-tmenu)
               (:binding "r" :command casual-ibuffer-mark-regexp-tmenu)
               (:binding "u" :command ibuffer-unmark-forward)
               (:binding "d" :command ibuffer-mark-for-delete)
               (:binding "x" :command ibuffer-do-kill-on-deletion-marks)
               (:binding "U" :command ibuffer-unmark-all-marks)

               (:binding "s" :command casual-ibuffer-sortby-tmenu)
               (:binding "`" :command ibuffer-switch-format)
               (:binding "b" :command ibuffer-bury-buffer)
               (:binding "g" :command ibuffer-update)
               ;;(:binding "$" :command ibuffer-toggle-filter-group)

               (:binding "p" :command ibuffer-backward-line)
               (:binding "n" :command ibuffer-forward-line)
               (:binding "{" :command ibuffer-backwards-next-marked)
               (:binding "}" :command ibuffer-forward-next-marked)
               (:binding "[" :command ibuffer-backward-filter-group)
               (:binding "]" :command ibuffer-forward-filter-group)
               (:binding "j" :command ibuffer-jump-to-buffer)
               ;; (:binding "M-j" :command ibuffer-jump-to-filter-group)


               (:binding "SPC" :command ibuffer-filter-chosen-by-completion)
               (:binding "/" :command ibuffer-filter-disable)
               (:binding "F" :command casual-ibuffer-filter-tmenu)

               (:binding "C-s" :command ibuffer-do-isearch)
               (:binding "C-M-s" :command ibuffer-do-isearch-regexp)
               (:binding "O" :command ibuffer-do-occur)
               (:binding "M-r" :command ibuffer-do-query-replace)
               (:binding "C-M-r" :command ibuffer-do-query-replace-regexp)

               (:binding "RET" :command casual-ibuffer-return-dwim)
               (:binding "," :command casual-ibuffer-settings-tmenu)
               (:binding "J" :command bookmark-jump)
               (:binding "q" :command quit-window)
               (:binding "^g" :command transient-quit-one))))

        (casualt-suffix-testcase-runner test-vectors
                                        #'casual-ibuffer-tmenu
                                        '(lambda () (random 5000)))))
    (casualt-ibuffer-breakdown)))

(ert-deftest test-casual-ibuffer-operations-tmenu ()
  (casualt-ibuffer-setup)
  (let ((test-vectors (list)))
    (push (casualt-suffix-test-vector "R" #'ibuffer-do-rename-uniquely) test-vectors)
    (push (casualt-suffix-test-vector "!" #'ibuffer-do-shell-command-file) test-vectors)
    (push (casualt-suffix-test-vector "|" #'ibuffer-do-shell-command-pipe) test-vectors)

    ;; (push (casualt-suffix-test-vector "E" #'ibuffer-do-eval) test-vectors)
    (push (casualt-suffix-test-vector "B" #'ibuffer-copy-buffername-as-kill) test-vectors)

    (push (casualt-suffix-test-vector "T" #'ibuffer-do-toggle-read-only) test-vectors)
    (push (casualt-suffix-test-vector "L" #'ibuffer-do-toggle-lock) test-vectors)

    (casualt-suffix-testbench-runner test-vectors
                                     #'casual-ibuffer-operations-tmenu
                                     '(lambda () (random 5000))))
  (casualt-ibuffer-breakdown t))

(ert-deftest test-casual-ibuffer-sortby-tmenu ()
  (casualt-ibuffer-setup)
  (let ((test-vectors (list)))
    (push (casualt-suffix-test-vector "v" #'ibuffer-do-sort-by-recency) test-vectors)
    (push (casualt-suffix-test-vector "a" #'ibuffer-do-sort-by-alphabetic) test-vectors)
    (push (casualt-suffix-test-vector "f" #'ibuffer-do-sort-by-filename/process) test-vectors)

    (push (casualt-suffix-test-vector "m" #'ibuffer-do-sort-by-major-mode) test-vectors)
    (push (casualt-suffix-test-vector "s" #'ibuffer-do-sort-by-size) test-vectors)

    (push (casualt-suffix-test-vector "i" #'ibuffer-invert-sorting) test-vectors)
    (push (casualt-suffix-test-vector "," #'ibuffer-toggle-sorting-mode) test-vectors)
    (casualt-suffix-testbench-runner test-vectors
                                     #'casual-ibuffer-sortby-tmenu
                                     '(lambda () (random 5000))))
  (casualt-ibuffer-breakdown t))

(ert-deftest test-casual-ibuffer-mark-tmenu ()
  (casualt-ibuffer-setup)
  (let ((test-vectors (list)))
    (push (casualt-suffix-test-vector "m" #'ibuffer-mark-by-mode) test-vectors)
    (push (casualt-suffix-test-vector "d" #'ibuffer-mark-dired-buffers) test-vectors)
    (push (casualt-suffix-test-vector "h" #'ibuffer-mark-help-buffers) test-vectors)

    (push (casualt-suffix-test-vector "*" #'ibuffer-mark-modified-buffers) test-vectors)
    (push (casualt-suffix-test-vector "r" #'ibuffer-mark-read-only-buffers) test-vectors)
    (push (casualt-suffix-test-vector "u" #'ibuffer-mark-unsaved-buffers) test-vectors)

    (push (casualt-suffix-test-vector "D" #'ibuffer-mark-dissociated-buffers) test-vectors)
    (push (casualt-suffix-test-vector "s" #'ibuffer-mark-special-buffers) test-vectors)
    (push (casualt-suffix-test-vector "z" #'ibuffer-mark-compressed-file-buffers) test-vectors)
    (push (casualt-suffix-test-vector "U" #'ibuffer-unmark-all-marks) test-vectors)

    (casualt-suffix-testbench-runner test-vectors
                                     #'casual-ibuffer-mark-tmenu
                                     '(lambda () (random 5000))))
  (casualt-ibuffer-breakdown t))

(ert-deftest test-casual-ibuffer-mark-regexp-tmenu ()
  (casualt-ibuffer-setup)
  (let ((test-vectors (list)))
    (push (casualt-suffix-test-vector "f" #'ibuffer-mark-by-file-name-regexp) test-vectors)
    (push (casualt-suffix-test-vector "n" #'ibuffer-mark-by-name-regexp) test-vectors)
    (push (casualt-suffix-test-vector "m" #'ibuffer-mark-by-mode-regexp) test-vectors)
    (push (casualt-suffix-test-vector "c" #'ibuffer-mark-by-content-regexp) test-vectors)
    (push (casualt-suffix-test-vector "U" #'ibuffer-unmark-all-marks) test-vectors)

    (casualt-suffix-testbench-runner test-vectors
                                     #'casual-ibuffer-mark-regexp-tmenu
                                     '(lambda () (random 5000))))
  (casualt-ibuffer-breakdown t))

(provide 'test-casual-ibuffer)
;;; test-casual-ibuffer.el ends here
