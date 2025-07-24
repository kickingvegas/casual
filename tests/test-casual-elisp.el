;;; test-casual-elisp.el --- Casual Make Tests -*- lexical-binding: t; -*-

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
(require 'casual-elisp-test-utils)
(require 'casual-lib-test-utils)
(require 'casual-elisp)

(ert-deftest test-casual-elisp-tmenu ()
  (let ((tmpfile "scratch-elisp.el"))
    (casualt-elisp-setup)
    (cl-letf (
              (casualt-mock #'eval-last-sexp)
              (casualt-mock #'elisp-eval-region-or-buffer)
              (casualt-mock #'eval-defun)
              (casualt-mock #'xref-find-definitions)
              (casualt-mock #'xref-find-references)
              (casualt-mock #'xref-find-references-and-replace)
              (casualt-mock #'checkdoc)
              (casualt-mock #'elisp-byte-compile-file)
              (casualt-mock #'elisp-byte-compile-buffer)
              (casualt-mock #'byte-recompile-directory)
              (casualt-mock #'find-library)
              (casualt-mock #'find-variable)
              (casualt-mock #'find-function)
              (casualt-mock #'backward-char)
              (casualt-mock #'backward-sexp)
              (casualt-mock #'forward-char)
              (casualt-mock #'casual-elisp-next-sexp)
              (casualt-mock #'previous-line)
              (casualt-mock #'backward-up-list)
              (casualt-mock #'next-line)
              (casualt-mock #'down-list)
              (casualt-mock #'casual-elisp-settings-tmenu)
              (casualt-mock #'transient-quit-all))

      (let ((test-vectors
             '((:binding "x" :command eval-last-sexp)
               (:binding "L" :command elisp-eval-region-or-buffer)
               (:binding "d" :command eval-defun)

               ;;(:binding "." :command xref-find-definitions)
               ;;(:binding "r" :command xref-find-references)
               ;;(:binding "R" :command xref-find-references-and-replace)

               (:binding "c" :command checkdoc)

               (:binding "B" :command elisp-byte-compile-file)
               (:binding "b" :command elisp-byte-compile-buffer)
               (:binding "D" :command byte-recompile-directory)

               (:binding "l" :command find-library)
               (:binding "v" :command find-variable)
               (:binding "f" :command find-function)

               (:binding "<left>" :command backward-char)
               (:binding "C-<left>" :command backward-sexp)
               (:binding "<right>" :command forward-char)
               (:binding "C-<right>" :command casual-elisp-next-sexp)
               (:binding "<up>" :command previous-line)
               (:binding "C-<up>" :command backward-up-list)
               (:binding "<down>" :command next-line)
               (:binding "C-<down>" :command down-list)
               (:binding "RET" :command transient-quit-all))))

        (casualt-suffix-testcase-runner test-vectors
                                        #'casual-elisp-tmenu
                                        '(lambda () (random 5000)))))
    (casualt-elisp-breakdown)))

(provide 'test-casual-elisp)
;;; test-casual-elisp.el ends here
