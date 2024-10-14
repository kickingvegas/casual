;;; test-casual-isearch.el --- Casual Re-Builder Tests -*- lexical-binding: t; -*-

;; Copyright (C) 2024  Charles Choi

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
(require 'casual-isearch-test-utils)
(require 'casual-lib-test-utils)
(require 'casual-isearch)

(ert-deftest test-casual-isearch-tmenu ()
  (let ((tmpfile "casual-isearch-tmenu.txt"))
    (casualt-setup)
    (cl-letf (;;((symbol-function #') (lambda () t))
              (casualt-mock #'isearch-edit-string)
              (casualt-mock #'isearch-yank-word-or-char)
              (casualt-mock #'isearch-yank-symbol-or-char)
              (casualt-mock #'isearch-yank-line)
              (casualt-mock #'isearch-yank-kill)
              (casualt-mock #'isearch-forward-thing-at-point)
              (casualt-mock #'isearch-query-replace)
              (casualt-mock #'isearch-query-replace-regexp)
              (casualt-mock #'isearch-toggle-case-fold)
              (casualt-mock #'isearch-toggle-lax-whitespace)
              (casualt-mock #'isearch-occur)
              (casualt-mock #'isearch-highlight-regexp)
              (casualt-mock #'isearch-highlight-lines-matching-regexp)
              (casualt-mock #'isearch-repeat-forward)
              (casualt-mock #'isearch-repeat-backward))

      (let ((test-vectors
             '((:binding "e" :command isearch-edit-string)
               (:binding "w" :command isearch-yank-word-or-char)
               (:binding "s" :command isearch-yank-symbol-or-char)
               (:binding "l" :command isearch-yank-line)
               (:binding "y" :command isearch-yank-kill)
               (:binding "t" :command isearch-forward-thing-at-point)

               (:binding "r" :command isearch-query-replace)
               (:binding "x" :command isearch-query-replace-regexp)

               (:binding "X" :command casual-isearch--toggle-regex-and-edit)
               (:binding "S" :command casual-isearch--toggle-symbol-and-edit)
               (:binding "W" :command casual-isearch--toggle-word-and-edit)
               (:binding "F" :command isearch-toggle-case-fold)
               (:binding "L" :command isearch-toggle-lax-whitespace)
               (:binding "o" :command isearch-occur)
               (:binding "h" :command isearch-highlight-regexp)
               (:binding "H" :command isearch-highlight-lines-matching-regexp)
               (:binding "n" :command isearch-repeat-forward)
               (:binding "p" :command isearch-repeat-backward)
               (:binding "RET" :command isearch-exit))))

        (casualt-suffix-testcase-runner test-vectors
                                        #'casual-isearch-tmenu
                                        '(lambda () (random 5000)))))
    (casualt-breakdown)))



(provide 'test-casual-isearch)
;;; test-casual-isearch.el ends here
