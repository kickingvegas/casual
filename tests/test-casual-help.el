;;; test-casual-help.el --- Casual Make Tests -*- lexical-binding: t; -*-

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
(require 'casual-lib-test-utils)
(require 'casual-help-test-utils)
(require 'casual-help)

(ert-deftest test-casual-help-tmenu ()
  (let ()
    (casualt-help-setup)

    (cl-letf ((casualt-mock #'beginning-of-buffer)
              (casualt-mock #'end-of-buffer)
              (casualt-mock #'previous-line)
              (casualt-mock #'next-line)
              (casualt-mock #'casual-lib-browse-backward-paragraph)
              (casualt-mock #'casual-lib-browse-forward-paragraph)
              (casualt-mock #'help-goto-previous-page)
              (casualt-mock #'help-goto-next-page)
              (casualt-mock #'help-go-back)
              (casualt-mock #'help-go-forward)
              (casualt-mock #'forward-button)
              (casualt-mock #'backward-button)
              (casualt-mock #'push-button)
              (casualt-mock #'describe-symbol)
              (casualt-mock #'describe-command)
              (casualt-mock #'describe-function)
              (casualt-mock #'describe-variable)

              (casualt-mock #'help-goto-info)
              (casualt-mock #'help-goto-lispref-info)

              (casualt-mock #'help-view-source)
              (casualt-mock #'help-customize)
              (casualt-mock #'casual-help-settings-tmenu)

              (casualt-mock #'quit-window))

      (let ((test-vectors
             '((:binding ">" :command end-of-buffer)
               (:binding "<" :command beginning-of-buffer)
               (:binding "C-n" :command next-line)
               (:binding "C-p" :command previous-line)
               (:binding "n" :command casual-lib-browse-forward-paragraph)
               (:binding "p" :command casual-lib-browse-backward-paragraph)
               (:binding "P" :command help-goto-previous-page)
               (:binding "N" :command help-goto-next-page)
               (:binding "M-[" :command help-go-back)
               (:binding "M-]" :command help-go-forward)

               (:binding "j" :command forward-button)
               (:binding "k" :command backward-button)
               (:binding "RET" :command push-button)

               ;; (:binding "ds" :command describe-symbol)
               ;; (:binding "dv" :command describe-variable)
               ;; (:binding "dc" :command describe-command)
               ;; (:binding "df" :command describe-function)

               (:binding "i" :command help-goto-info)
               (:binding "I" :command help-goto-lispref-info)

               (:binding "s" :command help-view-source)
               ;;(:binding "c" :command help-customize)

               (:binding "," :command casual-help-settings-tmenu)
               (:binding "q" :command quit-window)
               )))

        (casualt-suffix-testcase-runner test-vectors
                                        #'casual-help-tmenu
                                        '(lambda () (random 5000)))))
    (casualt-help-breakdown)))

(provide 'test-casual-help)
;;; test-casual-help.el ends here
