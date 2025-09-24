;;; test-casual-bibtex.el --- BibTeX Tests           -*- lexical-binding: t; -*-

;; Copyright (C) 2025  Charles Choi

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
(require 'casual-bibtex-test-utils)
(require 'casual-lib-test-utils)
(require 'casual-bibtex)

(ert-deftest test-casual-bibtex-tmenu ()
  (let ((tmpfile "casual-bibtex-main-tmenu.txt"))
    (casualt-bibtex-setup)
    (cl-letf ((casualt-mock #'bibtex-make-field)
              (casualt-mock #'bibtex-copy-field-as-kill)
              (casualt-mock #'casual-bibtex-copy-field-value)
              (casualt-mock #'bibtex-empty-field)
              (casualt-mock #'bibtex-kill-field)
              (casualt-mock #'bibtex-remove-OPT-or-ALT)
              (casualt-mock #'casual-bibtex-beginning-of-field)
              (casualt-mock #'casual-bibtex-end-of-field)
              (casualt-mock #'previous-line)
              (casualt-mock #'bibtex-next-field)
              (casualt-mock #'bibtex-entry)
              (casualt-mock #'bibtex-copy-entry-as-kill)
              (casualt-mock #'bibtex-kill-entry)
              (casualt-mock #'bibtex-entry-update)
              (casualt-mock #'bibtex-mark-entry)
              (casualt-mock #'bibtex-fill-entry)
              (casualt-mock #'bibtex-clean-entry)
              (casualt-mock #'bibtex-beginning-of-entry)
              (casualt-mock #'bibtex-end-of-entry)
              (casualt-mock #'bibtex-previous-entry)
              (casualt-mock #'bibtex-next-entry)
              (casualt-mock #'bibtex-yank)
              (casualt-mock #'bibtex-yank-pop)
              (casualt-mock #'bibtex-search-entries)
              (casualt-mock #'bibtex-search-crossref)
              (casualt-mock #'bibtex-sort-buffer)
              (casualt-mock #'bibtex-narrow-to-entry)
              (casualt-mock #'occur)
              (casualt-mock #'widen)
              (casualt-mock #'save-buffer)
              (casualt-mock #'bookmark-jump)
              )

      (let ((test-vectors
             '(
               ;; (:binding "a" :command bibtex-make-field)
               (:binding "c" :command bibtex-copy-field-as-kill)
               (:binding "w" :command casual-bibtex-copy-field-value)
               (:binding "x" :command bibtex-empty-field)
               (:binding "DEL" :command bibtex-kill-field)
               (:binding "o" :command bibtex-remove-OPT-or-ALT)
               (:binding "C-a" :command casual-bibtex-beginning-of-field)
               (:binding "C-e" :command casual-bibtex-end-of-field)
               (:binding "p" :command previous-line)
               (:binding "n" :command bibtex-next-field)

               (:binding "A" :command bibtex-entry)
               (:binding "C" :command bibtex-copy-entry-as-kill)
               (:binding "k" :command bibtex-kill-entry)
               (:binding "u" :command bibtex-entry-update)
               (:binding "m" :command bibtex-mark-entry)
               (:binding "f" :command bibtex-fill-entry)
               (:binding "C-c" :command bibtex-clean-entry)
               (:binding "<" :command bibtex-beginning-of-entry)
               (:binding ">" :command bibtex-end-of-entry)
               (:binding "M-p" :command bibtex-previous-entry)
               (:binding "M-n" :command bibtex-next-entry)

               (:binding "y" :command bibtex-yank)
               (:binding "M-y" :command bibtex-yank-pop)
               (:binding "/" :command bibtex-search-entries)
               (:binding "j" :command bibtex-search-entry)
               ;; (:binding "." :command bibtex-search-crossref)
               (:binding "s" :command bibtex-sort-buffer)
               (:binding "O" :command occur)
               (:binding "N" :command bibtex-narrow-to-entry)
               (:binding "C-s" :command save-buffer)
               (:binding "J" :command bookmark-jump)
               )))

        (casualt-suffix-testcase-runner test-vectors
                                        #'casual-bibtex-tmenu
                                        '(lambda () (random 5000)))))
    (casualt-bibtex-breakdown)))


(provide 'test-casual-bibtex)
;;; test-casual-bibtex.el ends here
