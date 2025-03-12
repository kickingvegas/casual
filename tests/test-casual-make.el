;;; test-casual-make.el --- Casual Make Tests -*- lexical-binding: t; -*-

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
(require 'casual-make-test-utils)
(require 'casual-lib-test-utils)
(require 'casual-make)

(ert-deftest test-casual-make-tmenu ()
  (let ((tmpfile "Makefile"))
    (casualt-make-setup tmpfile)
    (cl-letf ((casualt-mock #'makefile-backslash-region)
              (casualt-mock #'comment-region)
              (casualt-mock #'makefile-insert-target-ref)
              (casualt-mock #'makefile-insert-macro-ref)
              (casualt-mock #'makefile-insert-gmake-function)
              (casualt-mock #'makefile-pickup-everything)
              (casualt-mock #'makefile-pickup-filenames-as-targets)
              (casualt-mock #'makefile-create-up-to-date-overview)
              (casualt-mock #'makefile-previous-dependency)
              (casualt-mock #'makefile-next-dependency)
              (casualt-mock #'compile)
              (casualt-mock #'imenu))

      (let ((test-vectors
             '((:binding "\\" :command makefile-backslash-region)
               (:binding ";" :command comment-region)
               (:binding ":" :command makefile-insert-target-ref)
               (:binding "m" :command makefile-insert-macro-ref)
               (:binding "f" :command makefile-insert-gmake-function)
               (:binding "a" :command casual-make-automatic-variables-tmenu)

               (:binding "E" :command makefile-pickup-everything)
               (:binding "F" :command makefile-pickup-filenames-as-targets)

               (:binding "c" :command compile)
               (:binding "o" :command makefile-create-up-to-date-overview)
               (:binding "t" :command casual-make-mode-select-tmenu)
               (:binding "." :command casual-make-identify-autovar-region)

               ;; (:binding "i" :command imenu)
               (:binding "p" :command makefile-previous-dependency)
               (:binding "n" :command makefile-next-dependency))))

        (casualt-mock-active-region)
        (casualt-suffix-testcase-runner test-vectors
                                        #'casual-make-tmenu
                                        '(lambda () (random 5000)))))
    (casualt-make-breakdown tmpfile)))

(provide 'test-casual-make)
;;; test-casual-make.el ends here
