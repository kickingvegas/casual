;;; test-casual-compile.el --- Casual Make Tests -*- lexical-binding: t; -*-

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
(require 'casual-compile-test-utils)
(require 'casual-lib-test-utils)
(require 'casual-compile)

;; TODO: Need to figure out a way to deal with exiting out of grep.
;; (ert-deftest test-casual-compile-tmenu ()
;;   (let ((tempfile "foo"))
;;     ;;(casualt-compile-setup tempfile)

;;     (cl-letf ((casualt-mock #'previous-error-no-select)
;;               (casualt-mock #'next-error-no-select)
;;               (casualt-mock #'compilation-previous-error)
;;               (casualt-mock #'compilation-next-error)
;;               (casualt-mock #'compilation-display-error)
;;               (casualt-mock #'compile-goto-error)
;;               (casualt-mock #'compilation-previous-file)
;;               (casualt-mock #'compilation-next-file)
;;               (casualt-mock #'compile)
;;               (casualt-mock #'recompile)
;;               (casualt-mock #'kill-compilation)
;;               (casualt-mock #'casual-compile-settings-tmenu)
;;               (casualt-mock #'quit-window))

;;       (let ((test-vectors
;;              '((:binding "n" :command next-error-no-select)
;;                (:binding "p" :command previous-error-no-select)
;;                (:binding "j" :command compilation-next-error)
;;                (:binding "k" :command compilation-previous-error)
;;                (:binding "o" :command compilation-display-error)
;;                (:binding "RET" :command compile-goto-error)
;;                (:binding "[" :command compilation-previous-file)
;;                (:binding "]" :command compilation-next-file)
;;                ;; (:binding "c" :command compile)
;;                ;; (:binding "g" :command recompile)
;;                ;; (:binding "k" :command kill-compilation)
;;                (:binding "," :command casual-compile-settings-tmenu)
;;                (:binding "q" :command quit-window))
;;              ))

;;         (casualt-suffix-testcase-runner test-vectors
;;                                         #'casual-compile-tmenu
;;                                         '(lambda () (random 5000)))))
;;     ;; (casualt-compile-breakdown tempfile)
;;     ))

(provide 'test-casual-compile)
;;; test-casual-compile.el ends here
