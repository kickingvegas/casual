;;; test-casual-man.el --- Casual Make Tests -*- lexical-binding: t; -*-

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
(require 'casual-man-test-utils)
(require 'casual-lib-test-utils)
(require 'casual-man)

(ert-deftest test-casual-man-tmenu ()
  (let ()
    (casualt-man-setup)

    (cl-letf ((casualt-mock #'beginning-of-buffer)
              (casualt-mock #'end-of-buffer)
              (casualt-mock #'previous-line)
              (casualt-mock #'next-line)
              (casualt-mock #'casual-lib-browse-backward-paragraph)
              (casualt-mock #'casual-lib-browse-forward-paragraph)
              (casualt-mock #'casual-man-occur-options)
              (casualt-mock #'Man-previous-section)
              (casualt-mock #'Man-next-section)
              (casualt-mock #'Man-goto-section)
              (casualt-mock #'Man-goto-see-also-section)
              (casualt-mock #'Man-follow-manual-reference)
              (casualt-mock #'Man-previous-manpage)
              (casualt-mock #'Man-next-manpage)
              (casualt-mock #'Man-goto-page)
              (casualt-mock #'bookmark-set-no-overwrite)
              (casualt-mock #'bookmark-jump)
              (casualt-mock #'casual-man-info)
              (casualt-mock #'man)
              (casualt-mock #'Man-update-manpage)
              (casualt-mock #'Man-kill)
              (casualt-mock #'quit-window))

      (let ((test-vectors
             '((:binding "." :command beginning-of-buffer)
               (:binding ">" :command end-of-buffer)
               (:binding "C-n" :command next-line)
               (:binding "C-p" :command previous-line)
               (:binding "n" :command casual-lib-browse-forward-paragraph)
               (:binding "p" :command casual-lib-browse-backward-paragraph)
               (:binding "o" :command casual-man-occur-options)
               (:binding "]" :command Man-next-section)
               (:binding "[" :command Man-previous-section)
               (:binding "g" :command Man-goto-section)
               (:binding "s" :command Man-goto-see-also-section)
              ;; (:binding "r" :command Man-follow-manual-reference)
              ;; (:binding "M-n" :command Man-next-manpage)
              ;; (:binding "M-p" :command Man-previous-manpage)
              ;; (:binding "j" :command Man-goto-page)
               (:binding "B" :command bookmark-set-no-overwrite)
              ;; (:binding "J" :command bookmark-jump)
               ;; (:binding "m" :command man)
               (:binding "I" :command casual-man-info)
               (:binding "u" :command Man-update-manpage)
               (:binding "," :command casual-man-settings-tmenu)
               (:binding "K" :command Man-kill)
               (:binding "q" :command quit-window)
               )))

        (casualt-suffix-testcase-runner test-vectors
                                        #'casual-man-tmenu
                                        '(lambda () (random 5000)))))
    (casualt-man-breakdown)))

(provide 'test-casual-man)
;;; test-casual-man.el ends here
