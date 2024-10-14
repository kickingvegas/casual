;;; test-casual-re-builder.el --- Casual Re-Builder Tests -*- lexical-binding: t; -*-

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
(require 'casual-re-builder-test-utils)
(require 'casual-lib-test-utils)
(require 'casual-re-builder)

(ert-deftest test-casual-re-builder-tmenu-bindings ()
  (casualt-setup)
  (let ((test-vectors (list)))
    (push (casualt-suffix-test-vector "w" #'casual-re-builder-copy) test-vectors)
    (push (casualt-suffix-test-vector "g" #'casual-re-builder-grep-copy) test-vectors)
    (push (casualt-suffix-test-vector "c" #'reb-copy) test-vectors)
    (push (casualt-suffix-test-vector "p" #'reb-prev-match) test-vectors)
    (push (casualt-suffix-test-vector "n" #'reb-next-match) test-vectors)
    (push (casualt-suffix-test-vector "x" #'reb-change-syntax) test-vectors)
    (push (casualt-suffix-test-vector "b" #'reb-change-target-buffer) test-vectors)
    (push (casualt-suffix-test-vector "t" #'reb-toggle-case) test-vectors)
    (push (casualt-suffix-test-vector "s" #'reb-enter-subexp-mode) test-vectors)
    (push (casualt-suffix-test-vector "f" #'reb-force-update) test-vectors)
    (push (casualt-suffix-test-vector "," #'casual-re-builder-settings-tmenu) test-vectors)
    (push (casualt-suffix-test-vector "i" #'casual-re-builder-regexp-info) test-vectors)
    (push (casualt-suffix-test-vector "q" #'reb-quit) test-vectors)
    (casualt-suffix-testbench-runner test-vectors
                                     #'casual-re-builder-tmenu
                                     '(lambda () (random 5000))))
  (execute-kbd-macro "")
  (casualt-breakdown t))

(provide 'test-casual-re-builder)
;;; test-casual-re-builder.el ends here
