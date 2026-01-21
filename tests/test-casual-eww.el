;;; test-casual-eww.el --- Casual Make Tests -*- lexical-binding: t; -*-

;; Copyright (C) 2026  Charles Y. Choi

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
(require 'casual-eww-test-utils)
(require 'casual-lib-test-utils)
(require 'casual-eww)

(ert-deftest test-casual-eww-tmenu ()
  (let ()
    (casualt-eww-setup)

    (cl-letf (
              (casualt-mock #'eww-back-url)
              (casualt-mock #'eww-forward-url)
              (casualt-mock #'eww-list-histories)
              (casualt-mock #'eww-previous-url)
              (casualt-mock #'eww-next-url)
              (casualt-mock #'eww-up-url)
              (casualt-mock #'eww-top-url)
              (casualt-mock #'casual-lib-browse-backward-paragraph)
              (casualt-mock #'casual-lib-browse-forward-paragraph)
              (casualt-mock #'scroll-up-command)
              (casualt-mock #'scroll-down-command)
              (casualt-mock #'shr-previous-link)
              (casualt-mock #'shr-next-link)
              (casualt-mock #'eww-follow-link)
              (casualt-mock #'casual-eww-display-tmenu)
              (casualt-mock #'eww-readable)
              (casualt-mock #'eww)
              (casualt-mock #'eww-browse-with-external-browser)
              (casualt-mock #'eww-copy-page-url)
              (casualt-mock #'eww-copy-alternate-url)
              (casualt-mock #'eww-download)
              (casualt-mock #'eww-reload)
              (casualt-mock #'eww-add-bookmark)
              (casualt-mock #'eww-list-bookmarks)
              (casualt-mock #'eww-next-bookmark)
              (casualt-mock #'eww-previous-bookmark)
              (casualt-mock #'casual-eww-settings-tmenu)
              (casualt-mock #'casual-eww-info)
              (casualt-mock #'quit-window))

      (let ((test-vectors
             '((:binding "M-[" :command eww-back-url)
               (:binding "M-]" :command eww-forward-url)
               (:binding "H" :command eww-list-histories)
               (:binding "[" :command eww-previous-url)
               (:binding "]" :command eww-next-url)
               (:binding "^" :command eww-up-url)
               (:binding "t" :command eww-top-url)
               (:binding "p" :command casual-lib-browse-backward-paragraph)
               (:binding "n" :command casual-lib-browse-forward-paragraph)
               (:binding "SPC" :command scroll-up-command)
               (:binding "S-SPC" :command scroll-down-command)
               (:binding "k" :command shr-previous-link)
               (:binding "j" :command shr-next-link)
               (:binding "RET" :command eww-follow-link)
               (:binding "D" :command casual-eww-display-tmenu)
               (:binding "R" :command eww-readable)
               (:binding "M-l" :command eww)
               (:binding "&" :command eww-browse-with-external-browser)
               (:binding "c" :command eww-copy-page-url)
               (:binding "A" :command eww-copy-alternate-url)
               (:binding "d" :command eww-download)
               (:binding "g" :command eww-reload)
               (:binding "b" :command eww-add-bookmark)
               (:binding "B" :command eww-list-bookmarks)
               (:binding "M-n" :command eww-next-bookmark)
               (:binding "M-p" :command eww-previous-bookmark)
               (:binding "," :command casual-eww-settings-tmenu)
               (:binding "I" :command casual-eww-info)
               (:binding "q" :command quit-window))))

        (casualt-suffix-testcase-runner test-vectors
                                        #'casual-eww-tmenu
                                        '(lambda () (random 5000)))))
    (casualt-eww-breakdown)))


(ert-deftest test-casual-eww-bookmarks-tmenu ()
  (let ((tmpfile "casual-eww-bookmarks-tmenu.txt"))
    (casualt-eww-setup)
    (cl-letf ((casualt-mock #'eww-bookmark-kill)
              (casualt-mock #'eww-bookmark-yank)
              (casualt-mock #'eww-bookmark-browse)
              (casualt-mock #'previous-line)
              (casualt-mock #'next-line))

      (let ((test-vectors
             '((:binding "k" :command eww-bookmark-kill)
               (:binding "y" :command eww-bookmark-yank)
               (:binding "RET" :command eww-bookmark-browse)
               (:binding "p" :command previous-line)
               (:binding "n" :command next-line))))

        (casualt-suffix-testcase-runner test-vectors
                                        #'casual-eww-bookmarks-tmenu
                                        '(lambda () (random 5000)))))
    (casualt-eww-breakdown)))

(provide 'test-casual-eww)
;;; test-casual-eww.el ends here
