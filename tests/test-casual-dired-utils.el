;;; test-casual-dired-utils.el --- Casual Dired Utils Tests  -*- lexical-binding: t; -*-

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
(require 'casual-dired-test-utils)
(require 'casual-dired-utils)

(ert-deftest test-casual-dired-utils-tmenu-bindings ()
  (casualt-dired-setup)

  (let ((test-vectors (list)))
    (push (casualt-suffix-test-vector "F" #'dired-do-find-marked-files) test-vectors)
    (push (casualt-suffix-test-vector "Z" #'dired-do-compress) test-vectors)

    (push (casualt-suffix-test-vector "u" #'dired-upcase) test-vectors)
    (push (casualt-suffix-test-vector "d" #'dired-downcase) test-vectors)

    (push (casualt-suffix-test-vector "s" #'casual-dired-search-replace-tmenu) test-vectors)
    (push (casualt-suffix-test-vector "e" #'casual-dired-elisp-tmenu) test-vectors)
    (push (casualt-suffix-test-vector "l" #'casual-dired-link-tmenu) test-vectors)

    (casualt-suffix-testbench-runner test-vectors
                                     #'casual-dired-utils-tmenu
                                     '(lambda () (random 5000))))
  (casualt-dired-breakdown t))

(ert-deftest test-casual-dired-search-replace-tmenu-bindings ()
  (casualt-dired-setup)
  (cl-letf ((casualt-mock #'dired-do-find-regexp)
            (casualt-mock #'dired-do-isearch)
            (casualt-mock #'dired-do-isearch-regexp)
            (casualt-mock #'dired-do-search)
            (casualt-mock #'dired-do-query-replace-regexp)
            (casualt-mock #'dired-do-find-regexp-and-replace)
            (casualt-mock #'rgrep))

    (let ((test-vectors
           '((:binding "C-s" :command dired-do-isearch)
             (:binding "M-s" :command dired-do-isearch-regexp)
             (:binding "s" :command dired-do-search)
             (:binding "r" :command dired-do-query-replace-regexp)
             (:binding "g" :command dired-do-find-regexp)
             (:binding "G" :command dired-do-find-regexp-and-replace)
             (:binding "f" :command rgrep))))

      (casualt-suffix-testcase-runner test-vectors
                                      #'casual-dired-search-replace-tmenu
                                      '(lambda () (random 5000)))))
  (casualt-dired-breakdown))


(ert-deftest test-casual-dired-elisp-tmenu-bindings ()
  (casualt-dired-setup)

  (let ((test-vectors (list)))
    (push (casualt-suffix-test-vector "B" #'dired-do-byte-compile) test-vectors)
    (push (casualt-suffix-test-vector "L" #'dired-do-load) test-vectors)
    (push (casualt-suffix-test-vector "D" #'byte-recompile-directory) test-vectors)
    (push (casualt-suffix-test-vector "c" #'checkdoc-dired) test-vectors)
    (casualt-suffix-testbench-runner test-vectors
                                     #'casual-dired-elisp-tmenu
                                     '(lambda () (random 5000))))
  (casualt-dired-breakdown t))

(ert-deftest test-casual-dired-link-tmenu-bindings ()
  (casualt-dired-setup)

  (let ((test-vectors (list)))
    (push (casualt-suffix-test-vector "s" #'dired-do-symlink) test-vectors)
    (push (casualt-suffix-test-vector "S" #'dired-do-symlink-regexp) test-vectors)
    (push (casualt-suffix-test-vector "r" #'dired-do-relsymlink) test-vectors)
    (push (casualt-suffix-test-vector "R" #'dired-do-relsymlink-regexp) test-vectors)
    (push (casualt-suffix-test-vector "h" #'dired-do-hardlink) test-vectors)
    (push (casualt-suffix-test-vector "H" #'dired-do-hardlink-regexp) test-vectors)


    (casualt-suffix-testbench-runner test-vectors
                                     #'casual-dired-link-tmenu
                                     '(lambda () (random 5000))))
  (casualt-dired-breakdown t))

(ert-deftest test-casual-dired-unicode-get ()
  (let ((casual-lib-use-unicode nil))
    (should (string-equal (casual-dired-unicode-get :up-arrow) "Up"))
    (should (string-equal (casual-dired-unicode-get :down-arrow) "Down"))
    (should (string-equal (casual-dired-unicode-get :goto) "Goto"))
    (should (string-equal (casual-dired-unicode-get :directory) "Dir"))
    (should (string-equal (casual-dired-unicode-get :file) "File"))
    (should (string-equal (casual-dired-unicode-get :subdir) "Subdir")))

  (let ((casual-lib-use-unicode t))
    (should (string-equal (casual-dired-unicode-get :up-arrow) "↑"))
    (should (string-equal (casual-dired-unicode-get :down-arrow) "↓"))
    (should (string-equal (casual-dired-unicode-get :goto) "→"))
    (should (string-equal (casual-dired-unicode-get :directory) "📁"))
    (should (string-equal (casual-dired-unicode-get :file) "📄"))
    (should (string-equal (casual-dired-unicode-get :subdir) "🗂️"))))

(provide 'test-casual-dired-utils)
;;; test-casual-dired-utils.el ends here
