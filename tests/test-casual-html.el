;;; test-casual-html.el --- Casual HTML Tests -*- lexical-binding: t; -*-

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
(require 'casual-html-test-utils)
(require 'casual-lib-test-utils)
(require 'casual-html)

(ert-deftest test-casual-html-tmenu ()
  (let ((auto-insert-mode -1)
        (temp-file-name "casual-html-test.html"))
    (casualt-html-setup temp-file-name)

    (cl-letf ((casualt-mock #'sgml-tag)
              (casualt-mock #'sgml-attributes)
              (casualt-mock #'sgml-close-tag)
              (casualt-mock #'sgml-delete-tag)
              (casualt-mock #'casual-html-tags-tmenu)
              (casualt-mock #'sgml-skip-tag-backward)
              (casualt-mock #'sgml-skip-tag-forward)
              (casualt-mock #'backward-char)
              (casualt-mock #'forward-char)
              (casualt-mock #'next-line)
              (casualt-mock #'previous-line)
              (casualt-mock #'browse-url-of-buffer)
              (casualt-mock #'sgml-validate)
              (casualt-mock #'html-autoview-mode)
              (casualt-mock #'casual-html-settings-tmenu)
              (casualt-mock #'casual-html-info))

      (let ((test-vectors
             '((:binding "i" :command sgml-tag)
               (:binding "a" :command sgml-attributes)
               (:binding "c" :command sgml-close-tag)
               ;;(:binding "d" :command sgml-delete-tag) ; need to handle html-ts-mode
               (:binding "h" :command casual-html-tags-tmenu)
               (:binding "b" :command browse-url-of-buffer)
               (:binding "v" :command sgml-validate)
               (:binding "A" :command html-autoview-mode)
               (:binding "h" :command casual-html-tags-tmenu)
               (:binding "[" :command sgml-skip-tag-backward)
               (:binding "]" :command sgml-skip-tag-forward)
               (:binding "C-f" :command forward-char)
               (:binding "C-b" :command backward-char)
               (:binding "C-n" :command next-line)
               (:binding "C-p" :command previous-line)
               (:binding "I" :command casual-html-info)
               (:binding "," :command casual-html-settings-tmenu)
               (:binding "TAB" :command sgml-tags-invisible))))

        (casualt-suffix-testcase-runner test-vectors
                                        #'casual-html-tmenu
                                        '(lambda () (random 5000)))))
    (casualt-html-breakdown temp-file-name)))

(ert-deftest test-casual-html-tags-tmenu ()
  (let ((auto-insert-mode -1)
        (temp-file-name "casual-html-tags-test.html"))
    (casualt-html-setup temp-file-name)

    (cl-letf ((casualt-mock #'html-paragraph)
              (casualt-mock #'html-image)
              (casualt-mock #'html-line)
              (casualt-mock #'html-horizontal-rule)
              (casualt-mock #'html-div)
              (casualt-mock #'html-href-anchor)
              (casualt-mock #'html-href-anchor-file)
              (casualt-mock #'html-id-anchor)
              (casualt-mock #'html-name-anchor)
              (casualt-mock #'html-ordered-list)
              (casualt-mock #'html-unordered-list)
              (casualt-mock #'html-list-item)
              (casualt-mock #'html-radio-buttons)
              (casualt-mock #'html-checkboxes)
              (casualt-mock #'html-headline-1)
              (casualt-mock #'html-headline-2)
              (casualt-mock #'html-headline-3)
              (casualt-mock #'html-headline-4)
              (casualt-mock #'html-headline-5)
              (casualt-mock #'html-headline-6)

              (casualt-mock #'facemenu-set-bold)
              (casualt-mock #'facemenu-set-italic)
              (casualt-mock #'facemenu-set-underline)
              (casualt-mock #'facemenu-set-bold-italic)
              (casualt-mock #'facemenu-set-default)
              (casualt-mock #'facemenu-set-face)

              (casualt-mock #'sgml-skip-tag-backward)
              (casualt-mock #'sgml-skip-tag-forward)
              (casualt-mock #'backward-char)
              (casualt-mock #'forward-char)
              (casualt-mock #'next-line)
              (casualt-mock #'previous-line)

              )

      (let ((test-vectors
             '(
               (:binding "p" :command html-paragraph)
               (:binding "i" :command html-image)
               (:binding "b" :command html-line)
               (:binding "hr" :command html-horizontal-rule)
               (:binding "d" :command html-div)
               (:binding "aa" :command html-href-anchor)
               (:binding "af" :command html-href-anchor-file)
               (:binding "ai" :command html-id-anchor)
               (:binding "an" :command html-name-anchor)
               (:binding "o" :command html-ordered-list)
               (:binding "u" :command html-unordered-list)
               (:binding "l" :command html-list-item)
               (:binding "r" :command html-radio-buttons)
               (:binding "c" :command html-checkboxes)
               ;; (:binding "1" :command html-headline-1)
               ;; (:binding "2" :command html-headline-2)
               ;; (:binding "3" :command html-headline-3)
               ;; (:binding "4" :command html-headline-4)
               ;; (:binding "5" :command html-headline-5)
               ;; (:binding "6" :command html-headline-6)

               (:binding "fb" :command facemenu-set-bold)
               (:binding "fi" :command facemenu-set-italic)
               (:binding "fu" :command facemenu-set-underline)
               (:binding "fl" :command facemenu-set-bold-italic)
               (:binding "fd" :command facemenu-set-default)
               (:binding "ff" :command facemenu-set-face)

               (:binding "[" :command sgml-skip-tag-backward)
               (:binding "]" :command sgml-skip-tag-forward)
               (:binding "C-f" :command forward-char)
               (:binding "C-b" :command backward-char)
               (:binding "C-n" :command next-line)
               (:binding "C-p" :command previous-line))))

        (casualt-suffix-testcase-runner test-vectors
                                        #'casual-html-tags-tmenu
                                        '(lambda () (random 5000)))))
    (casualt-html-breakdown temp-file-name)))

(provide 'test-casual-html)
;;; test-casual-html.el ends here
