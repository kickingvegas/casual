;;; test-casual-dired.el --- Casual Dired Tests      -*- lexical-binding: t; -*-

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
(require 'casual-dired-test-utils)
(require 'casual-dired)

(ert-deftest test-casual-dired-tmenu-bindings ()
  (casualt-dired-setup)

  (cl-letf
      (((symbol-function #'dired-do-shell-command) (lambda (x) (interactive)(print "WARNING: override")))
       ((symbol-function #'dired-do-async-shell-command) (lambda (x) (interactive)(print "WARNING: override")))
       ((symbol-function #'image-dired) (lambda (x) (interactive)(print "WARNING: override"))))

    (let ((test-vectors (list))
          (dired-use-ls-dired t))
      (push (casualt-suffix-test-vector "o" #'dired-find-file-other-window) test-vectors)
      (push (casualt-suffix-test-vector "v" #'dired-view-file) test-vectors)
      (push (casualt-suffix-test-vector "C" #'dired-do-copy) test-vectors)
      (push (casualt-suffix-test-vector "R" #'dired-do-rename) test-vectors)
      (push (casualt-suffix-test-vector "D" #'dired-do-delete) test-vectors)
      (push (casualt-suffix-test-vector "l" #'casual-dired-link-tmenu) test-vectors)
      (push (casualt-suffix-test-vector "c" #'casual-dired-change-tmenu) test-vectors)
      (push (casualt-suffix-test-vector "y" #'dired-show-file-type) test-vectors)
      (push (casualt-suffix-test-vector "w" #'dired-copy-filename-as-kill) test-vectors)
      (push (casualt-suffix-test-vector "!" #'dired-do-shell-command) test-vectors)
      (push (casualt-suffix-test-vector "&" #'dired-do-async-shell-command) test-vectors)
      (push (casualt-suffix-test-vector "W" #'browse-url-of-dired-file) test-vectors)

      (push (casualt-suffix-test-vector "s" #'casual-dired-sort-by-tmenu) test-vectors)
      (push (casualt-suffix-test-vector "h" #'dired-hide-details-mode) test-vectors)
      (push (casualt-suffix-test-vector "O" #'dired-omit-mode) test-vectors)
      ;; (push (casualt-suffix-test-vector "iq" #'dired-maybe-insert-subdir 2) test-vectors)
      (push (casualt-suffix-test-vector "$" #'dired-hide-subdir) test-vectors)
      (push (casualt-suffix-test-vector "k" #'dired-do-kill-lines) test-vectors)
      (push (casualt-suffix-test-vector "g" #'revert-buffer) test-vectors)
      (push (casualt-suffix-test-vector "f" #'casual-dired-find-dired-regexp) test-vectors)
      (push (casualt-suffix-test-vector "E" #'wdired-change-to-wdired-mode) test-vectors)
      ;; (push (casualt-suffix-test-vector "T" #'image-dired) test-vectors) ; breaks in command line because not display-graphic-p
      (push (casualt-suffix-test-vector "m" #'dired-mark) test-vectors)
      (push (casualt-suffix-test-vector "u" #'dired-unmark) test-vectors)
      (push (casualt-suffix-test-vector "U" #'dired-unmark-all-marks) test-vectors)
      (push (casualt-suffix-test-vector "t" #'dired-toggle-marks) test-vectors)
      (push (casualt-suffix-test-vector "~" #'dired-flag-backup-files) test-vectors)
      (push (casualt-suffix-test-vector "x" #'dired-do-flagged-delete) test-vectors)
      (push (casualt-suffix-test-vector "r" #'casual-dired-regexp-tmenu) test-vectors)

      (push (casualt-suffix-test-vector "^" #'dired-up-directory) test-vectors)
      (push (casualt-suffix-test-vector "p" #'dired-previous-line) test-vectors)
      (push (casualt-suffix-test-vector "n" #'dired-next-line) test-vectors)
      (push (casualt-suffix-test-vector "ð" #'dired-prev-dirline) test-vectors)
      (push (casualt-suffix-test-vector "î" #'dired-next-dirline) test-vectors)
      (push (casualt-suffix-test-vector "j" #'dired-goto-file) test-vectors)
      (push (casualt-suffix-test-vector "ê" #'dired-goto-subdir) test-vectors)
      ;;(push (casualt-suffix-test-vector "[" #'dired-prev-subdir) test-vectors)
      ;;(push (casualt-suffix-test-vector "]" #'dired-next-subdir) test-vectors)

      (push (casualt-suffix-test-vector "J" #'bookmark-jump) test-vectors)
      (push (casualt-suffix-test-vector "B" #'bookmark-set-no-overwrite) test-vectors)
      (push (casualt-suffix-test-vector "b" #'ibuffer) test-vectors)

      ;;(push (casualt-suffix-test-vector "" dired-isearch-filenames) test-vectors)
      (push (casualt-suffix-test-vector "ó" #'dired-isearch-filenames-regexp) test-vectors)
      (push (casualt-suffix-test-vector "/" #'casual-dired-search-replace-tmenu) test-vectors)

      ;;(push (casualt-suffix-test-vector "+q" #'dired-create-directory) test-vectors)
      (push (casualt-suffix-test-vector "F" #'dired-create-empty-file) test-vectors)

      (casualt-suffix-testbench-runner test-vectors
                                       #'casual-dired-tmenu
                                       '(lambda () (random 5000)))))
  (casualt-dired-breakdown t))


(ert-deftest test-casual-dired-regexp-tmenu-bindings ()
  (casualt-dired-setup)
  (let ((test-vectors (list)))
    (push (casualt-suffix-test-vector "m" #'dired-mark-files-regexp) test-vectors)
    (push (casualt-suffix-test-vector "c" #'dired-mark-files-containing-regexp) test-vectors)
    (push (casualt-suffix-test-vector "d" #'dired-flag-files-regexp) test-vectors)
    (push (casualt-suffix-test-vector "C" #'dired-do-copy-regexp) test-vectors)
    (push (casualt-suffix-test-vector "r" #'dired-do-rename-regexp) test-vectors)

    (casualt-suffix-testbench-runner test-vectors
                                     #'casual-dired-regexp-tmenu
                                     '(lambda () (random 5000))))
  (casualt-dired-breakdown t))

(ert-deftest test-casual-dired-change-tmenu-bindings ()
  (casualt-dired-setup)
  (let ((test-vectors (list)))
    (push (casualt-suffix-test-vector "M" #'dired-do-chmod) test-vectors)
    (push (casualt-suffix-test-vector "G" #'dired-do-chgrp) test-vectors)
    (push (casualt-suffix-test-vector "O" #'dired-do-chown) test-vectors)
    (push (casualt-suffix-test-vector "T" #'dired-do-touch) test-vectors)

    (casualt-suffix-testbench-runner test-vectors
                                     #'casual-dired-change-tmenu
                                     '(lambda () (random 5000))))
  (casualt-dired-breakdown t))


(ert-deftest test-casual-dired-format-arrow ()
  (should (string-equal (casual-dired-format-arrow "hey" t) " hey"))
  (should (string-equal (casual-dired-format-arrow "hey" nil) "hey")))

(provide 'test-casual-dired)
;;; test-casual-dired.el ends here
