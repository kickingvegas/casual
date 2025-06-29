;;; test-casual-eshell.el --- Casual Make Tests -*- lexical-binding: t; -*-

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
(require 'casual-eshell)
(require 'casual-lib-test-utils)
(require 'casual-eshell-test-utils)

(ert-deftest test-casual-eshell-tmenu ()
  (let ()
    (casualt-eshell-setup)

    (cl-letf ((casualt-mock #'eshell-insert-buffer-name)
              (casualt-mock #'eshell-kill-input)
              (casualt-mock #'eshell-list-history)
              (casualt-mock #'eshell-backward-argument)
              (casualt-mock #'eshell-forward-argument)
              (casualt-mock #'eshell-repeat-argument)
              (casualt-mock #'eshell-previous-prompt)
              (casualt-mock #'eshell-next-prompt)
              (casualt-mock #'eshell-copy-old-input)
              (casualt-mock #'eshell-show-output)
              (casualt-mock #'eshell-show-maximum-output)
              (casualt-mock #'eshell-mark-output)
              (casualt-mock #'eshell-delete-output)
              ;;(casualt-mock #'dired)
              (casualt-mock #'casual-eshell-edit-aliases)

              (casualt-mock #'casual-eshell-info-tmenu)
              (casualt-mock #'casual-eshell-settings-tmenu)
              ;;(casualt-mock #'bookmark-jump)
              ;;(casualt-mock #'magit-status)
              )

      (let ((test-vectors
             '((:binding "B" :command eshell-insert-buffer-name)
               (:binding "k" :command eshell-kill-input)
               (:binding "h" :command eshell-list-history)
               (:binding "b" :command eshell-backward-argument)
               (:binding "f" :command eshell-forward-argument)
               (:binding "y" :command eshell-repeat-argument)
               (:binding "p" :command eshell-previous-prompt)
               (:binding "n" :command eshell-next-prompt)
               (:binding "RET" :command eshell-copy-old-input)
               (:binding "s" :command eshell-show-output)
               (:binding "." :command eshell-show-maximum-output)
               (:binding "m" :command eshell-mark-output)
               (:binding "D" :command eshell-delete-output)
               ;; (:binding "d" :command dired)
               (:binding "a" :command casual-eshell-edit-aliases)
               (:binding "i" :command casual-eshell-info-tmenu)
               (:binding "," :command casual-eshell-settings-tmenu))
             ))

        (casualt-suffix-testcase-runner test-vectors
                                        #'casual-eshell-tmenu
                                        '(lambda () (random 5000)))))
    (casualt-eshell-breakdown)))


(ert-deftest test-casual-eshell-info-tmenu ()
  (let ()
    (casualt-eshell-setup)
    (cl-letf ((casualt-mock #'casual-eshell-info)
              (casualt-mock #'casual-eshell-info-builtins)
              (casualt-mock #'casual-eshell-info-aliases)
              (casualt-mock #'casual-eshell-info-remote-access)
              (casualt-mock #'casual-eshell-info-control-flow)
              (casualt-mock #'casual-eshell-info-expansion)
              (casualt-mock #'casual-eshell-info-dollars-expansion)
              (casualt-mock #'casual-eshell-info-redirection)
              (casualt-mock #'casual-eshell-info-pipelines))

      (let ((test-vectors
             '(
               (:binding "i" :command casual-eshell-info)
               (:binding "b" :command casual-eshell-info-builtins)
               (:binding "a" :command casual-eshell-info-aliases)
               (:binding "r" :command casual-eshell-info-remote-access)
               (:binding "c" :command casual-eshell-info-control-flow)
               (:binding "e" :command casual-eshell-info-expansion)
               (:binding "d" :command casual-eshell-info-dollars-expansion)
               (:binding "R" :command casual-eshell-info-redirection)
               (:binding "p" :command casual-eshell-info-pipelines)
               )))

        (casualt-suffix-testcase-runner test-vectors
                                        #'casual-eshell-info-tmenu
                                        '(lambda () (random 5000)))))
    (casualt-eshell-breakdown)))

(provide 'test-casual-eshell)
;;; test-casual-eshell.el ends here
